namespace FsGame

open Core

module Touch =

    //
    // --------- Types ---------
    //

    type Touch = Touch of int * Point


    type GestureSample = GestureSample of GameTime * Touch


    type TouchEventType =
        | TouchDown of Point
        | TouchMoved of Point
        | TouchUp of Point


    type TouchEvent = int * GameTime * TouchEventType


    type PendingGestureType =
        | Tap of GameTime * Point
        | Drag of Point


    type PendingGesture = int * PendingGestureType


    type GestureType =
        | TouchDown of Point
        | TouchUp of Point
        | Tap of Point
        | Drag of Point * Delta


    type Gesture = int * GestureType


    type EventProcessResult =
        | PendingGesture of PendingGesture
        | Gesture of Gesture




    //
    // --------- Functions ---------
    //

    let bimapT2 f1 f2 t2 =
        let a,b = t2
        (f1 a, f2 b)


    let fullJoin list1 list2 key1Selector key2Selector =
        let notInList list keySelector = (fun x -> List.map keySelector list |> List.contains x |> not)
        let list1MissingKeys = List.filter (notInList list1 key1Selector) (list2 |> List.map key2Selector)
        let list2MissingKeys = List.filter (notInList list2 key2Selector) (list1 |> List.map key1Selector)
        let list1Full =
            (list1 |> List.map Some |> List.zip (list1 |> List.map key1Selector))
            @ (list1MissingKeys |> List.map (fun i -> i,None))
            |> List.sortBy fst
        let list2Full =
            (list2 |> List.map Some |> List.zip (list2 |> List.map key2Selector))
            @ (list2MissingKeys |> List.map (fun i -> i,None))
            |> List.sortBy fst
        List.zip list1Full list2Full |> List.map (bimapT2 snd snd)


    let dragThreshold = 5.0


    let gestureSample gameTime touch  = GestureSample ( gameTime , touch )


    let id ( Touch ( id , _ ) ) = id


    let position ( Touch ( _ , pos ) ) = pos


    let touchEvent gameTime = function
        | ( Some ( Touch _ ) ),( Some ( Touch ( id , pos ) ) ) ->
            [ id, gameTime, TouchEventType.TouchMoved pos ]

        | (Some (Touch (id,pos))),None ->
            [ id, gameTime, TouchEventType.TouchUp pos ]

        | None,(Some (Touch (id,pos))) ->
            [ id, gameTime, TouchEventType.TouchDown pos ]

        | _ -> []



    let touchEvents previousTouches newTouches gameTime =
        let length = List.length newTouches
        fullJoin previousTouches newTouches id id
        |> List.collect (touchEvent gameTime)


    let processEvent = function
        | _,(Some (id,gameTime,TouchEventType.TouchDown position)) -> 
                [
                Gesture(id,TouchDown position)
                PendingGesture(id,PendingGestureType.Tap (gameTime,position))
                ] 

        | (Some (_,(PendingGestureType.Tap (tapStartTime, startPos)))),(Some (id,time,TouchEventType.TouchUp endPos)) 
            when time.total.Subtract(tapStartTime.total).Milliseconds < 500 && distance startPos endPos < dragThreshold ->
                [
                Gesture(id,TouchUp endPos)
                Gesture(id,Tap endPos)
                ]

        | (Some (_,(PendingGestureType.Tap (tapStartTime, startPos)))),(Some (id,time,TouchMoved endPos)) 
            when time.total.Subtract(tapStartTime.total).Milliseconds < 500 && distance startPos endPos < dragThreshold ->
                [PendingGesture(id,PendingGestureType.Tap (tapStartTime,startPos))]

        | (Some (_,(PendingGestureType.Tap (tapStartTime, startPos)))),(Some (id,time,TouchMoved endPos)) 
            when time.total.Subtract(tapStartTime.total).Milliseconds >= 500 && distance startPos endPos < dragThreshold ->
                [PendingGesture(id,PendingGestureType.Drag (startPos))]

        | (Some (_,(PendingGestureType.Tap (_, startPos)))),(Some (id,_,TouchEventType.TouchUp endPos)) 
            when distance startPos endPos >= dragThreshold ->
                [
                Gesture(id,TouchUp endPos)
                Gesture(id,Drag (startPos,(subtract startPos endPos)))
                ]

        | (Some (_,(PendingGestureType.Tap (_, startPos)))),(Some (id,_,TouchMoved endPos)) 
            when distance startPos endPos >= dragThreshold ->
                [
                Gesture(id,Drag (startPos,(subtract startPos endPos)))
                PendingGesture(id, PendingGestureType.Drag (endPos))
                ]

        | (Some (_,(PendingGestureType.Drag startPos))),(Some (id,_,TouchMoved endPos)) ->
            [
            Gesture(id,Drag (startPos,(subtract startPos endPos)))
            PendingGesture(id, PendingGestureType.Drag (endPos))
            ]

        | (Some (_,(PendingGestureType.Drag startPos))),(Some (id,_,TouchEventType.TouchUp endPos)) ->
            [
            Gesture(id,Drag (startPos,(subtract startPos endPos)))
            Gesture(id,TouchUp endPos)
            PendingGesture(id, PendingGestureType.Drag (endPos))
            ]

        | _,(Some (id,_,TouchEventType.TouchUp pos)) -> [Gesture(id,TouchUp pos)]

        | _ -> [] 


    let processEvents (previousPending : PendingGesture list) (events : TouchEvent list) = 
        fullJoin previousPending events fst (fun (id,_,_) -> id) 
        |> List.collect processEvent


    let gestures = List.collect (function Gesture x -> [x] | _ -> [])


    let pendingGestures = List.collect (function PendingGesture x -> [x] | _ -> [])