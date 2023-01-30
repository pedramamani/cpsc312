import Test.Tasty
import Test.Tasty.HUnit
import qualified Config
import qualified Base
import qualified Data.Set as Set

main :: IO ()
main = defaultMain $ testGroup "Tests"
    [ testCase "Activity" $ do
        let t = Base.Activity "CPSC 312"
        Base.unparse t @?= "CPSC 312",

      testCase "isNotBetween" $ do
        let d = Base.Date 2021 12 06
        Base.isBetween (Base.Date 2021 12 03) (Base.Date 2021 12 04) d @?= False,

      testCase "todayIsNotBetween" $ do
        d <- Base.today
        Base.isBetween (Base.Date 2021 12 03) (Base.Date 2021 12 04) d @?= False,

      testCase "isBetween" $ do
        let d = Base.Date 2021 12 06
        Base.isBetween (Base.Date 2021 12 03) (Base.Date 2021 12 14) d @?= True,

      testCase "Duration1" $ do
        let t = Base.Record (Base.Time 19 12) (Base.Time 20 25) (Base.Date 2021 12 06) (Base.Activity "Learn")
        Base.duration t @?= 73,

      testCase "Duration2" $ do
        let t = Base.Record (Base.Time 19 22) (Base.Time 20 16) (Base.Date 2021 12 06) (Base.Activity "Learn")
        Base.duration t @?= 54,

      testCase "parseStringToRecord" $ do
        let s = "19:12 | 19:13 | 2021-12-06 | Learn"
        Base.parse s @?= Base.Record (Base.Time 19 12) (Base.Time 19 13) (Base.Date 2021 12 06) (Base.Activity "Learn"),

      testCase "parseTree1" $ do
        let s = "1\n    2"
        Base.parse s @?= Base.Node (Base.Activity "1") [Base.Node (Base.Activity "2") []],
        
      testCase "parseTree2" $ do
        let s = "1\n    2\n    3"
        Base.parse s @?= Base.Node (Base.Activity "1") [Base.Node (Base.Activity "2") [], Base.Node (Base.Activity "3") []],
        
      testCase "parseTree3" $ do
        let s = "1\n    2\n        3"
        Base.parse s @?= Base.Node (Base.Activity "1") [Base.Node (Base.Activity "2") [Base.Node (Base.Activity "3") []]],

      testCase "unparseRecordToString" $ do
        let d = Base.Record (Base.Time 19 12) (Base.Time 19 13) (Base.Date 2021 12 16) (Base.Activity "Learn")
        Base.unparse d @?= "19:12 | 19:13 | 2021-12-16 | Learn",

      testCase "unparseTree1" $ do
        let t = Base.Node (Base.Activity "1") [Base.Node (Base.Activity "2") []]
        Base.unparse t @?= "1\n    2",

      testCase "unparseTree2" $ do
        let t = Base.Node (Base.Activity "1") [Base.Node (Base.Activity "2") [], Base.Node (Base.Activity "3") []]
        Base.unparse t @?= "1\n    2\n    3",
        
      testCase "unparseTree3" $ do
        let t = Base.Node (Base.Activity "1") [Base.Node (Base.Activity "2") [Base.Node (Base.Activity "3") []]]
        Base.unparse t @?= "1\n    2\n        3",

      testCase "allKeys" $ do
        let n = Base.Node 1 [Base.Node 2 [], Base.Node 3 [Base.Node 4 []]]
        Base.allKeys n @?= Set.fromList[1,2,3,4],

      testCase "find1" $ do
        let a = Base.Activity "1"
        let t = Base.Node (Base.Activity "1") []
        Base.find a t @?= Just t,

      testCase "find2" $ do
        let a = Base.Activity "3"
        let t = Base.Node (Base.Activity "1") [Base.Node (Base.Activity "2") [], Base.Node (Base.Activity "3") []]
        Base.find a t @?= Just (Base.Node (Base.Activity "3") []),

      testCase "find3" $ do
        let a = Base.Activity "3"
        let t = Base.Node (Base.Activity "1") [Base.Node (Base.Activity "2") [Base.Node (Base.Activity "3") []]]
        Base.find a t @?= Just (Base.Node (Base.Activity "3") []),

      testCase "findfail" $ do
        let a = Base.Activity "4"
        let t = Base.Node (Base.Activity "1") [Base.Node (Base.Activity "2") [Base.Node (Base.Activity "3") []]]
        Base.find a t @?= Nothing
    ]
