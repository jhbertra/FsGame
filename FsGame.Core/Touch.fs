namespace FsGame

open Core
open FsEssentials.Prelude

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