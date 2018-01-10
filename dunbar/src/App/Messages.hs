module App.Messages (
  module App.Messages
) where

import Data.List (intercalate)

mainMenu :: String
mainMenu = intro ++ (intercalate ("\n" ++ padding) options)
  where options = [ "(n)ew friends"
                  , "(v)iew friends"
                  , "(s)how friend"
                  , "(d)elete friend"
                  , "(q)uit"
                  ]
        intro = "Choose option: "
        padding = take (length intro) $ repeat ' '

enterFriendId = "Enter ID of friend to view: "
enterDeleteFriendId = "Enter ID of friend to delete:"
friendDoesNotExist idS = "Friend does not exist for ID: " ++ idS
enterFirstname = "Enter firstname: "
enterLastname = "Enter lastname: "
enterNote = "Enter a note (optional): "
emptyFirstname = "Firstname cannot be empty - please try again"
emptyLastname = "Lastname cannot be empty - please try again"
goodbye = "Byyeeeee!!!"

continuePaging = "Type <enter> for more, m to escape"
