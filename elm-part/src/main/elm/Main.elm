import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
-- import Json.Encode as Encode

main : Program Never Model Msg
main =
  Html.program
    { init = start
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

url : String -> String
url action = "http://localhost:9000/member/" ++ action

-- MODEL

type alias Model =
  { count : Int
  , message : String
  , member : Member
  }

type alias Member =
  { id : Int
    ,name : String
    , email : String
  }

start : (Model, Cmd Msg)
start =
  ( Model 0 "No message" (Member 1 "Thomas" "Thim")
  , Cmd.none
  )

-- UPDATE

type Msg
  = GetMemberCount
  | MemberCountReceived (Result Http.Error Int)
  | GetMember
  | MemberReceived (Result Http.Error Member)
  | ChangeId String
  | ChangeName String
  | ChangeEmail String
  | PostMember
  | MemberPosted (Result Http.Error String)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GetMemberCount ->
      (model, getMemberCount)

    MemberCountReceived (Ok newCount) ->
      ( { model | count = newCount }, Cmd.none)

    MemberCountReceived (Err error) ->
      ( { model | message = toString error }, Cmd.none)

    GetMember ->
      (model, getMember model.member.id)

    MemberReceived (Ok newMember) ->
      ({ model | member = newMember}, Cmd.none)

    MemberReceived (Err error) ->
      ( { model | message = toString error }, Cmd.none)

    ChangeId idText -> --one way to modify only one field of member is to do this
      case String.toInt idText of
        Ok id ->
          let
            oldMember = model.member
          in
            ({ model | member = {oldMember | id = id}}, Cmd.none)
        Err _ -> -- also possible this way where we construct a brand new member with Member constructor
          ({model | member = (Member 0 model.member.name model.member.email), message = "Fail to parse ID, set to 0"}, Cmd.none)

    ChangeName newName ->

        ({ model | member = (Member model.member.id newName model.member.email)}, Cmd.none)

    ChangeEmail newEmail ->
        ({ model | member = (Member model.member.id model.member.name newEmail)}, Cmd.none)

    PostMember ->
      (model, postMember model.member)

    MemberPosted (Err error) ->
      ({model | message = toString error}, Cmd.none)

    MemberPosted (Ok msg) ->
      ({model | message = msg}, getMemberCount)


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ h2 [] [text ("Member Count = " ++ toString model.count) ]
    , button [ onClick GetMemberCount ] [ text "Update Member Count" ]
    , button [ onClick GetMember ] [ text "get a Member"]
    , hr [] []
    , input [ type_ "text", value (toString  model.member.id), onInput ChangeId ] []
    , input [ type_ "text", value model.member.name, onInput ChangeName ] []
    , input [ type_ "text", value model.member.email, onInput ChangeEmail ] []
    , button [ onClick PostMember] [text "post a member"]
    ,hr [] []
    , text model.message
    ]


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

-- HTTP

getMemberCount : Cmd Msg
getMemberCount =
    Http.send MemberCountReceived (Http.get (url "count") Decode.int)

getMember : Int -> Cmd Msg
getMember id =
  Http.send MemberReceived (Http.get (url (toString id)) decodeMember)

memberJsonBody : Member -> Http.Body
memberJsonBody member =
  Http.jsonBody <| encodeMember member --piping

postMember : Member -> Cmd Msg
postMember member =
  Http.send MemberPosted (Http.post (url "")(memberJsonBody member) Decode.string)

decodeMember : Decode.Decoder Member
decodeMember =
  Decode.map3 Member
  (Decode.field "id" Decode.int)
  (Decode.field "name" Decode.string)
  (Decode.field "email" Decode.string)

encodeMember : Member -> Encode.Value
encodeMember member =
  Encode.object
      [ ("id", Encode.int member.id)
      , ("name", Encode.string member.name)
      , ("email", Encode.string member.email)
      ]
