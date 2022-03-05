data RegExp a = Nil
                | Eps
                | Atom a
                | Sum (RegExp a) (RegExp a)
                | Seq (RegExp a) (RegExp a)
                | Star (RegExp a)
    deriving Show

nullable :: RegExp a -> Bool
nullable Nil       = False
nullable Eps       = True
nullable (Atom _)  = False
nullable (Sum f g) = nullable f || nullable g
nullable (Seq f g) = nullable f && nullable g
nullable (Star _)  = True

derive :: Eq a => RegExp a -> a -> RegExp a
derive Nil _ = Nil
derive Eps _ = Nil
derive (Atom b) a   | b /= a = Nil
                    | otherwise = Eps
derive (Sum f g) a = Sum (derive f a) (derive g a)
derive (Seq f g) a  | nullable f = Sum (Seq (derive f a) g) (derive g a)
                    | otherwise = Seq (derive f a) g
derive (Star f) a = Seq (derive f a) (Star f)

match :: Eq a => RegExp a -> [a] -> Bool
match reg = nullable . foldl derive reg

empty :: RegExp a -> Bool
empty Nil = True 
empty (Sum f g) = empty f && empty g
empty (Seq f g) = empty f || empty g
empty _ = False

normalize :: RegExp a -> RegExp a
normalize Nil = Nil
normalize Eps = Eps
normalize (Atom a) = Atom a
normalize (Seq f g) =
    case (normalize f, normalize g) of
        (Nil, _) -> Nil
        (_, Nil) -> Nil
        (f, g)   -> Seq f g
normalize (Sum f g) =
    case (normalize f, normalize g) of
        (Nil, g) -> g
        (f, Nil) -> f
        (f, g)   -> Sum f g
normalize (Star f) =
    case normalize f of
        Nil -> Eps
        f   -> Star f
