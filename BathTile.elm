import Keyboard (arrows)
import Automaton (state,run)

colorTable = [grey, red, yellow]

registerColor n = case n of
                    1 -> grey
                    2 -> red
                    3 -> yellow

boxData = [[1,2,3,1,1,2,1,2,3],[3,2,1,2,3,2,1,2,3]]

rowToForm = group . rowToForm'

--Need to make this tail recursive
rowToForm' : [Int] -> [Form]
rowToForm' xs = if xs == []
                  then []
                  else (filled (registerColor (head xs)) (rect 8 8)) :: (map (move (10,0)) (rowToForm' (tail xs)))

stackForms : [Form] -> Form
stackForms = group . stackForms'

stackForms' xs = if xs == []
                 then []
                 else head xs :: map (move (0,10)) (tail xs)

plotData input = [move input <| filled black (rect 12 12), 
                  stackForms (map rowToForm boxData)]

main = lift (collage 300 300 . plotData) positionAutomaton


arrowToPosition : {a | x : Int, y : Int} -> (Float,Float) -> (Float,Float)
arrowToPosition input state =  (fst state + 10 * toFloat input.x,
                                snd state + 10 * toFloat input.y)



positionAutomaton : Signal (Float,Float)
positionAutomaton = run ( state (0,0) arrowToPosition) (0,0) arrows
