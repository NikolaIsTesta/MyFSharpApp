open System.IO
open Microsoft.FSharp.Reflection

    type Product(price:int, firm:string) =
            member val Price = 
                    if int price < 0 then
                            failwith("Цена не может быть отрицательной") 
                        else int price with get, set
            member val Firm = firm
            member this.PrintPerson() = printfn $"Цена: {this.Price} Фирма: {this.Firm}"


    type Mouse(parameters: string list) =
        inherit Product(int parameters.[2], parameters.[3])
        member val Style = 
            if (parameters.[0] <> "шариковая" && parameters.[0] <> "лазерная") then
                failwith("Неверный тип мышки")
            else parameters.[0] with get, set
        member val Count = 
            if int parameters.[1] < 0 then
                   failwith("Количество мышек не может быть отрицательным")
            else int parameters.[1] with get, set
        override this.ToString() =
            sprintf "%s %s %d %d %s" 
                (this.GetType().Name) this.Style this.Count base.Price base.Firm

    type Keyboard(parameters: string list) =
        inherit Product(int parameters.[2], parameters.[3])
        member val Style = 
            if (parameters.[0] <> "мембранная" && parameters.[0] <> "купольная" && parameters.[0] <> "емкостная") then
                failwith("Неверный тип мышки")
            else parameters.[0] with get, set
        member val Connector = parameters.[1] with get, set
        override this.ToString() =
            sprintf "%s %s %s %d %s" 
                (this.GetType().Name) this.Style this.Connector base.Price base.Firm

    type Monitor(parameters: string list) =
        inherit Product(int parameters.[2], parameters.[3])
        member val Height = 
            if int parameters.[0] < 0 then
                failwith("Высота монитора не может быть отрицательной")
            else int parameters.[0] with get, set
        member val Width = 
            if int parameters.[1] < 0 then
                failwith("Ширина монитора не может быть отрицательной")
            else int parameters.[1] with get, set
        override this.ToString() =
            sprintf "%s %d %d %d %s" 
                (this.GetType().Name) this.Height this.Width base.Price base.Firm

    type Node(value: Product, next: Node option) =
        member val value = value with get
        member val Next : Node option = next with get, set

    type CircularLinkedList() =
        let mutable head = None
        let mutable tail = None
        let mutable count: int = 0

        member this.Add(product: Product) =
            let node = new Node(product, None)
            match tail with
            | None ->
                head <- Some node
                tail <- Some node
                node.Next <- Some node
                count <- count + 1
            | Some t ->
                node.Next <- head
                t.Next <- Some node
                tail <- Some node
                count <- count + 1

        member this.Print() =
            let mutable currentNode = head
            let writer = new System.IO.StreamWriter("output.txt")
            while currentNode <> None && currentNode <> tail do
                printfn "%A" currentNode.Value.value
                writer.WriteLine(currentNode.Value.value.ToString())
                currentNode <- currentNode.Value.Next
            printfn "%A" tail.Value.value
            writer.WriteLine(tail.Value.value.ToString())
            writer.Close()

        member this.rem(parameters: string list) =  
            let mutable currentNode : Node option = head
            let mutable coune_list : int = count    
            while coune_list > 0 do
                for cur in currentNode.Value.value.GetType().GetProperties() do   
                    match cur.Name with   
                    | value when value = parameters.[0] ->    
                        match parameters.[1] with   
                        | "==" ->   
                            if (cur.GetValue(currentNode.Value.value).ToString()) = string(parameters.[2]) then   
                                this.removeNode currentNode
                        | ">=" ->   
                            if (cur.GetValue(currentNode.Value.value) :?> int) >= int(parameters.[2]) then   
                                this.removeNode currentNode
                        | "<=" ->   
                            if (cur.GetValue(currentNode.Value.value) :?> int) <= int(parameters.[2]) then   
                                this.removeNode currentNode
                        | "<" ->   
                            if (cur.GetValue(currentNode.Value.value) :?> int) < int(parameters.[2]) then   
                                this.removeNode currentNode
                        | ">" ->   
                            if (cur.GetValue(currentNode.Value.value) :?> int) > int(parameters.[2]) then  
                                this.removeNode currentNode
                        | "!=" ->   
                            if (cur.GetValue(currentNode.Value.value).ToString()) <> string(parameters.[2]) then  
                                this.removeNode currentNode
                        | _ ->   
                            failwith $"Неверное условие: {parameters.[1]}" 
                    | _ ->   
                        ()   
                currentNode <- currentNode.Value.Next
                coune_list <- coune_list - 1
        

        member this.removeNode (nodeToRemove: Node option) =  
            let mutable currentNode : Node option = head     
            let mutable prevNode : Node option = None  
            while currentNode <> None do  
                if (currentNode = nodeToRemove) then 
                    match prevNode with  
                    | None ->  
                        head <- currentNode.Value.Next  
                        tail.Value.Next <- head
                        count <- count - 1
                        currentNode <- None
                    | Some p ->  
                        p.Next <- currentNode.Value.Next  
                        if currentNode = head then 
                            head <- p.Next 
                        if currentNode = tail then  
                            tail <- Some(p)
                        count <- count - 1
                        currentNode <- None
                if currentNode <> None then
                    prevNode <- currentNode 
                    currentNode <- currentNode.Value.Next
                    if currentNode = head then
                        currentNode <- None
        

let list = new CircularLinkedList()

let readCommands (fileName: string) =
    let commands = ref []
    use file = new StreamReader(fileName)
    while not file.EndOfStream do
        let line = file.ReadLine()
        let parts : string [] = line.Split([|' '|], System.StringSplitOptions.RemoveEmptyEntries)
        match parts.[0] with
        | "ADD" -> commands := ("ADD", Array.tail parts |> Array.toList) :: !commands
        | "REM" -> commands := ("REM", Array.tail parts |> Array.toList) :: !commands
        | "PRINT" -> commands := ("PRINT", []) :: !commands
        | _ -> printfn $"Неверная команда: {parts.[0]}" 
    List.rev !commands


let add(parameters: string list) =
    match parameters.[0] with
    | "Мышки" -> list.Add(new Mouse(List.tail parameters))
    | "Клавиатуры" -> list.Add(new Keyboard(List.tail parameters))
    | "Мониторы" -> list.Add(new Monitor(List.tail parameters))
    | _ -> printfn $"Неверный тип продукта: {parameters.[0]}"

let executeCommands (commands: (string * string list) list) =
    for cmd in commands do
        match cmd with
        | ("ADD", item) -> add item
        | ("REM", item) -> list.rem(item)
        | ("PRINT", _) -> list.Print()
        | _ -> ()

let fileName = "task-list.txt"
let commands = readCommands fileName
executeCommands commands