module PersistHandlers where
import Yesod

mkYesodData "TodoApp" $(parseRoutesFile "config/routes")
mkYesodDispatch "TodoApp" resourcesApp

postTodosR :: Handler String
postTodosR = do
    todo <- requireJsonBody
    todoId <- runDB $ insert (todo :: Todo)
    sendSuccess Entity {
        entityKey = todoId
        , entityVal = todo
    }

getTodoR :: Key Todo -> Handler String
getTodoR key = do
    todo <- runDB $ selectList [TodoId ==. key] [LimitTo 1]
    if length todo == 1 then sendSuccess $ todo !! 0
    else sendBadReq $ ErrBody "Todo does not exist" False

putTodoR :: Key Todo -> Handler String
putTodoR todoId = do
    todo <- requireJsonBody
    updated <- runDB $ repsert todoId (todo :: Todo)
    sendSuccess Entity {
        entityKey = todoId
        , entityVal = todo
    }
        

getTodosR :: Handler String 
getTodosR = do
    entries <- runDB $ selectList [] [Desc TodoOrder]
    sendSuccess entries