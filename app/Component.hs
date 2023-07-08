module Component(
  Component,
  ComponentType,
  setUnion,
  getUnionValue,
  initComponentFromType,
  setStructElement,
  getStructElement
)where


-- public interface


newtype Component = Component { getComp :: Comp } deriving Show

setUnion :: Component -> String -> Component -> Component
setUnion union variant value = Component $ setUnionComp (getComp union) variant (getComp value)

getUnionValue :: Component -> Component
getUnionValue = Component . getUnionCompValue . getComp

initComponentFromType :: ComponentType -> Component
initComponentFromType = Component . initCompFromType

setStructElement :: Component -> String -> Component -> Component
setStructElement struct element value = Component $ setStructCompElement (getComp struct) element (getComp value)

getStructElement ::Component -> String -> Component 
getStructElement struct element = Component $ getStructCompElement (getComp struct) element
data ComponentType = IntegerType | BooleanType | StructType String [(String, ComponentType)] | UnionType String [(String, ComponentType)] deriving (Show, Eq)


-- private interface
data CompData = Integer Int | Boolean Bool | Struct [(String, Comp)] | Union String Comp deriving Show

data Comp = Comp ComponentType CompData deriving Show


setUnionComp :: Comp -> String -> Comp -> Comp
setUnionComp (Comp (UnionType x typelist) (Union _ _)) variant (Comp comptype compdata) 
  | (variant, comptype) `elem` typelist =
    Comp
      (UnionType x typelist)
      (Union variant (Comp comptype compdata))

getUnionCompValue :: Comp -> Comp
getUnionCompValue (Comp _ (Union _ x)) = x


initCompFromType :: ComponentType -> Comp
initCompFromType IntegerType = Comp IntegerType $ Integer 0
initCompFromType BooleanType = Comp BooleanType $ Boolean False
initCompFromType (StructType name list) = Comp (StructType name list) (Struct $ map (\(x, y) -> (x, initCompFromType y)) list)
initCompFromType (UnionType name list) = Comp (UnionType name list) (Union headtype headcomp)
  where 
    headtype = fst $ head list
    headcomp = initCompFromType $ snd $ head list

setStructCompElement :: Comp -> String -> Comp -> Comp
setStructCompElement (Comp (StructType name types) (Struct comps)) element (Comp _type value)
  | (element, _type) `elem` types = 
    Comp 
      (StructType name types) 
      (Struct  $ (element , Comp _type value) : filter (\(x, y) -> x /= element) comps)

getStructCompElement :: Comp -> String -> Comp
getStructCompElement (Comp _ (Struct list)) element
  | element `elem` ( map fst list ) = snd $ head $ filter (\(x, _) -> x == element) list   



