{- ----------------------------------------------------------------------------------------
   what    : Various Unicode output and encodings
   expected: ok
---------------------------------------------------------------------------------------- -}

module Main where


main = do
  putStrLn $      "ABCD"
  putStrLn $ show "ABCD"

  putStrLn $      "čušpajž日本語"

  -- same strings
  putStrLn $      "\3619\3657\3634\3609\3648\3592\3657\3648\3621\3657\3591"
  putStrLn $      "ร้านเจ้เล้ง"
  putStrLn $ show "\3619\3657\3634\3609\3648\3592\3657\3648\3621\3657\3591"

  putStrLn $ show "\255"

