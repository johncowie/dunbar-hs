module App.Messages (
  module App.Messages
) where

import Data.List (intercalate)

buildMenu :: String -> [String] -> String
buildMenu intro options = intro ++ (intercalate ("\n" ++ padding) options)
  where padding = take (length intro) $ repeat ' '

mainMenu :: String
mainMenu = buildMenu "Choose option: "
                    [ "(n)ew friends"
                    , "(v)iew friends"
                    , "(s)how friend"
                    , "(q)uit"]

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

friendMenu = buildMenu "Choose option: "
                       [ "(n) - add note"
                       , "(d) - delete friend"
                       , "(q) - return to main menu"]

addNote = "Enter note: "

emptyNote = "Note cannot be empty - please try again"

invalidOption = "Invalid option - please try again"
