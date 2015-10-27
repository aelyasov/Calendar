module CalendarTemplate where

import System.Environment (getArgs)


main :: IO ()
main = do args <- getArgs
          -- let year = (read $ head args) :: Int
          let year = 2015
              calendar = calendarForYear year
          putStrLn $ showCalendar calendar


type Year     = Int -- 0..
type Month    = Int -- 1..12
type Day      = Int -- 1..31
type Weekday  = Int -- 0..6
type Calendar = (Year, [Weekday]) -- Number for first weekday of each month

monthNames :: [String]
monthNames = [ "January"   , "February" , "March"    , "April"
             , "May"       , "June"     , "July"     , "August"
             , "September" , "October"  , "November" , "December"]


weekdays :: [String]
weekdays = ["su","mo","tu","we","th","fr","sa"]


calendarForYear :: Year -> Calendar
calendarForYear year = (year, [ firstDayOfMonth year m | m <- [1..12] ])


-- | Excercise: Fix isLeapYear by using 4 guards
isLeapYear :: Year -> Bool
isLeapYear year = error "'isLeapYear' is undefined"
                where
                    devides :: Int -> Int -> Bool
                    devides n y = y `mod` n == 0


daysPerMonth :: Year -> [Day]
daysPerMonth year = [31, if isLeapYear year then 29 else 28 ,31,30,31,30,31,31,30,31,30,31]


daysInMonth :: Month -> Year -> Day
daysInMonth month year = daysPerMonth year !! (month-1)


showCalendar :: Calendar -> String
showCalendar ( year, firstdays ) = unlines . concat . separateBy horizontal . map besides . makeGroupsOf 3 . map showMonth $ [1..12]
    where
        showMonth :: Month -> [String]
        showMonth m = calendarMonth year m $ firstdays !! (m - 1)

        horizontal :: [String]
        horizontal = [concat (separateBy "+" (replicate 3 (replicate 22 '-')))]



calendarMonth :: Month -> Year -> Weekday -> [String]
calendarMonth year month firstday = title : body
    where
        title :: String
        title = cjustify 22 (monthNames !! (month-1) <> " " <> show year)

        body :: [String]
        body = take 6 . map (concat . separateBy " ") $ makeGroupsOf 7 boxes
            where
                 -- boxes are 2 character strings
                 boxes      = weekdays <> startSpace <> days <> endSpace
                 startSpace = replicate firstday "  "
                 days       = map (rjustify 2 . show) [1..daysInMonth month year]
                 endSpace   = repeat "  "



firstDayOfMonth :: Year -> Month -> Int
firstDayOfMonth year month  = sum ( year : nrOfLeapYears : daysThisYear ) `mod` 7
    where
        -- | Exercise: Define function daysThisYear that returns the list of the days for each month for the given year as a list comprehension (use function daysInMonth)
        daysThisYear :: [ Day ]
        daysThisYear = error "'daysThisYear' is undefined"

        -- | Exercise: Remove the duplication of year-1 from nrOfLeapYears by using a where clause
        nrOfLeapYears :: Int
        nrOfLeapYears = ((year-1) `div` 4) - ((year-1) `div` 100) + ((year-1) `div` 400)


-- | Exercise: Implement functions  rjustify, cjustify :: Int -> String -> String which can do right and center justification respectively
-- | Example: rjustify 5 “ab” = “   ab”, cjustify 5 “ab” = “  ab ”
-- | Hint: you may need to use the function replicate 
rjustify :: Int -> String -> String
rjustify i s = error "'rjustify' is undefined"


cjustify :: Int -> String -> String
cjustify i s = error "'cjustify' is undefined"


-- | Exercise: Implement function separateBy :: a -> [a] -> [a], which given separator ‘|’ and a list “abc” returns a new list where all elements are separated, i.e. "|a|b|c|"
-- | Hint: you may want to use concat or concatMap
separateBy :: a -> [a] -> [a]
separateBy sep xs = error "'separateBy' is undefined"


-- | Exercise: Implement  function makeGroupsOf :: Int -> [a] -> [[a]], which splits given list into groups of a provided size, i.e. makeGroupsOf 2 [1,2,3,4,5] = [[1,2], [3,4], [5]]
-- | Hint: you may want to use functions take and drop
makeGroupsOf :: Int -> [a] -> [[a]]
makeGroupsOf = error "'makeGroupsOf' is undefined"


-- | Exercise*: Implement function besides :: [[String]] -> [String], besides [["a", "b"], ["c", "d"], ["e", "f"]] = ["a | c | e", "b | d | f"]
-- | Hint: you may need to use functions foldr1, zipWith and repeat
besides :: [[String]] -> [String]
besides xss = error "'besides' is undefined"

-- | Exercise: Using foldl define infix operator (<>) :: [a] -> [a] -> [a] that appends to list
-- | Exercise*: define (<>) as foldl
-- | Hint: we ask you to reimplement (++) from Prelude
(<>) :: [a] -> [a] -> [a]
xs <> ys = error "'(<>)' is undefined"


-- | Exercise: create module `StringUtils` that exposes functions rjustify, cjustify, separateBy, makeGroupsOf, besides, (<>)

-- | Exercise: write function myfoldl :: (a -> b -> a) -> a -> [b] > a
myfoldl :: (a -> b -> a ) -> a -> [b] -> a
myfoldl = error "'myfoldl' is undefined"

-- | Exercise*: define function myfoldl' in terms of foldr
myfoldl' :: (a -> b -> a ) -> a -> [b] -> a
myfoldl' = error "'myfoldl'' is undefined"
