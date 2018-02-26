namespace FsGame

open FsEssentials
open Prelude

open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Audio
open Microsoft.Xna.Framework.Input.Touch

open Core
open Touch

module iOS =

    let vector2ToFloatTuple (vec : Microsoft.Xna.Framework.Vector2) = ((float)vec.X,(float)vec.Y)
        
    let scale = 750.0 / float GraphicsAdapter.DefaultAdapter.CurrentDisplayMode.Width // temporary hard-coded scaling

    let gameSpace = (*) scale

    let screenSpace x = x / scale

    let getTouches (touchCol: TouchCollection) =
        touchCol
        |> List.ofSeq
        |> List.map (fun touch -> 
            Touch (touch.Id,touch.Position |> vector2ToFloatTuple |> mapT2 gameSpace |> Point ))

    type TouchGame<'m, 't>(engine : GameEngine<'m, Touch list, 't>) as this =
        inherit Microsoft.Xna.Framework.Game()


        let _ = new Microsoft.Xna.Framework.GraphicsDeviceManager(this)


        [<DefaultValue>] val mutable model : 'm
        [<DefaultValue>] val mutable delayed : ( System.TimeSpan * float * Update<'m, Touch list, 't> ) list
        [<DefaultValue>] val mutable objects : GameObject<'t> list
        [<DefaultValue>] val mutable textures : Map<string, Texture2D>
        [<DefaultValue>] val mutable sfxInstances : Map<string, SoundEffectInstance>
        [<DefaultValue>] val mutable sfx : Map<string, SoundEffect>
        [<DefaultValue>] val mutable spriteBatch : SpriteBatch


        let rec runCommands totalTime = function
            | [] -> ()
            | PlaySound (sound, mode, volume)::xs ->
                Option.maybe {
                    let! sfx = this.sfxInstances.TryFind sound
                    if sfx.State = SoundState.Stopped then
                        sfx.Volume <- float32 volume
                        sfx.Play() |> ignore
                    else
                        match mode with
                        | Overlap -> 
                                let! sfx = this.sfx.TryFind sound
                                let instance = sfx.CreateInstance()
                                instance.Volume <- float32 volume
                                instance.Play() |> ignore
                        | NoOverlap -> () |> ignore
                } |> ignore
                runCommands totalTime xs
            | Delay ( delay , update ) :: xs ->
                this.delayed <- List.rev ( ( totalTime , delay , update ) :: List.rev this.delayed )
                runCommands totalTime xs



        //
        // --------- Initialize ---------
        //

        override this.Initialize() = 
            this.spriteBatch <- new SpriteBatch(this.GraphicsDevice)
            this.model <- engine.init
            this.delayed <- []
            TouchPanel.EnabledGestures <- Microsoft.Xna.Framework.Input.Touch.GestureType.Tap ||| GestureType.FreeDrag ||| GestureType.DragComplete
            base.Initialize()



        //
        // --------- Load ---------
        //

        let findTexture t = Map.find t this.textures


        let rec makeBox (findTexture : string -> Texture2D) (Point (x,y)) = function
            | [] -> BoundingBox (x, y, 0.0, 0.0)
            | t::ts ->
                let texture = findTexture t
                let tBox = BoundingBox (x, y, float texture.Width, float texture.Height)
                makeBox findTexture (Point (x,y)) ts |> union tBox


        let toGameObject findTexture = function
            | Area (texture, point, tag) -> { textures = []; box = makeBox findTexture point [texture]; alpha = 1.0; tag = tag }
            | Sprite (textures, point, alpha, tag) -> { textures = textures; box = makeBox findTexture point textures; alpha = alpha; tag = tag }
            

        override this.LoadContent() =
            let manifest = engine.contentManifest
            this.Content.RootDirectory <- manifest.root
            this.textures <- 
                manifest.textures
                |> List.fold (fun m t -> Map.add t (this.Content.Load<Texture2D>("textures/" + t)) m) (Map[])
            let sfxList = manifest.sfx |> List.map (fun s -> (s,this.Content.Load<SoundEffect>("sfx/" + s)))
            this.sfxInstances <- 
                sfxList
                |> List.fold (fun m (s,fx) -> Map.add s (fx.CreateInstance()) m) (Map[])
            this.sfx <- 
                sfxList
                |> List.fold (fun m (s,fx) -> Map.add s (fx) m) (Map[])
            this.objects <- engine.draw this.model |> List.map (toGameObject findTexture)
            base.LoadContent()



        //
        // --------- Update ---------
        //


        let rec runDelayed gameState = function
        | [] -> State.fromValue gameState.model
        | update :: updates ->
                let (UpdateResult (model,newCommands)) = update gameState
                State.modify (newCommands |> flip (@))
                |> State.bind ( fun () -> runDelayed { gameState with model = model } updates )


        override this.Update(gameTime) =
            let touches = getTouches (TouchPanel.GetState())

            let gameState = { 
                controller = touches
                gameTime = 
                    { 
                    elapsed = gameTime.ElapsedGameTime
                    total = gameTime.TotalGameTime
                    isRunningSlowly = gameTime.IsRunningSlowly 
                    }
                objects = this.objects
                model = this.model 
                }

            let elapsedDelays = 
                this.delayed
                |> List.takeWhile ( fun ( start : System.TimeSpan , wait , _ ) ->
                    (gameTime.TotalGameTime.TotalMilliseconds - start.TotalMilliseconds) * 0.001 >= wait )
                |> List.map ( fun ( _ , _ , update ) -> update )

            let ( commands , model ) = 
                State.state {
                    let! model = runDelayed gameState elapsedDelays
                    let ( UpdateResult ( model , newCommands ) ) = engine.update gameState
                    do! State.modify (newCommands |> flip (@))
                    return model
                }
                |> State.run []

            
            this.delayed <- List.skip ( List.length elapsedDelays ) this.delayed

            this.model <- model
            runCommands gameTime.TotalGameTime commands                
            base.Update(gameTime)



        //
        // --------- Draw ---------
        //

        override this.Draw gameTime = 
            this.objects <- engine.draw this.model |> List.map (toGameObject findTexture)
            this.spriteBatch.Begin(SpriteSortMode.Immediate, BlendState.AlphaBlend)
            for object in this.objects do
                for texture in object.textures do
                    let texture2D = findTexture texture
                    let x,y,w,h = bounds object |> mapT4 screenSpace |> mapT4 int
                    this.spriteBatch.Draw(texture2D, Microsoft.Xna.Framework.Rectangle(x, y, w, h), Microsoft.Xna.Framework.Color.White * float32 object.alpha)
            this.spriteBatch.End()
            base.Draw(gameTime)