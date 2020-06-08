import GAMvector

main :: IO ()

-- null vector (E1 + E2)/sqrt(2) + F1 squares to ~0.0
vec = mv [(1/sqrt(2),"E1")] + mv [(1/sqrt(2),"E2")] + mv [(1,"F1")]
main = putStrLn (show (vec^2))