module EsqueltoHandlers where
import Database.Esqueleto as E
import YesodInstance as YI

patchTodo key todo = update $ \p -> do
    set p [ TodoOrder =. coalesce [val . patchOrder $ todo, p ^. TodoOrder]
        , TodoText =. coalesceDefault [val . patchText $ todo] (p ^. TodoText)
        , TodoComplete =. coalesceDefault [val . patchComplete $ todo] (p ^. TodoComplete) ]
    where_ (p ^. TodoId ==. val key)
    

patchTodoR :: Key Todo -> Handler String
patchTodoR key = do
    todo <- requireJsonBody
    _ <- runDB $ patchTodo key todo 
    getTodoR key