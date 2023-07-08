module Component where
import Data.Fixed (E0)

data ComponentType = IntegerType | BooleanType | StructType String [(String, ComponentType)] | UnionType String [(String, ComponentType)] deriving (Show, Eq)

data CompData = Integer Int | Boolean Bool | Struct [(String, Comp)] | Union String Comp deriving Show

data Comp = Comp ComponentType CompData deriving Show

newtype Component = Component { getComp :: Component } deriving Show

setUnion :: Comp -> String -> Comp -> Comp
setUnion (Comp x y) (Comp (UnionType name xs) _ )
 | x `elem`  map snd xs = Comp (UnionType name xs) (Union x (Comp x y))

getUnionValue :: Comp -> Comp
getUnionValue (Comp _ (Union _ x)) = x


initCompFromType :: ComponentType -> Comp
initCompFromType IntegerType = Comp IntegerType $ Integer 0
initCompFromType BooleanType = Comp BooleanType $ Boolean False
initCompFromType (StructType name list) = Comp (StructType name list) (Struct $ map (\(x, y) -> (x, initCompFromType y)) list)
initCompFromType (UnionType name list) = Comp (UnionType name list) (Union headtype headcomp)
  where 
    headtype = fst $ head list
    headcomp = initCompFromType $ snd $ head list

setStructElement :: Comp -> String -> Comp -> Comp
setStructElement (Comp (StructType name types) (Struct comps)) element (Comp _type value)
  | (element, _type) `elem` types = 
    Comp 
      (StructType name types) 
      (Struct  $ (element , Comp _type value) : filter (\(x, y) -> x /= element) comps)

