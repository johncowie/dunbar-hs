import qualified AppSpec as AppSpec
import qualified StoreSpec as StoreSpec
import qualified Utils.ListSpec as ListSpec
import qualified Data.FriendSpec as FriendSpec

main :: IO ()
main = do
  AppSpec.main
  StoreSpec.main
  ListSpec.main
  FriendSpec.main
