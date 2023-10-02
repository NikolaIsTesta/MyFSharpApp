  member this.rem(parameters: string list) =
            let firm = parameters |> List.filter (fun p -> p.StartsWith("firm=")) |> List.map (fun p -> p.Substring(5))
            match firm with
            | [] -> ()
            | [firmName] ->
                let mutable currentNode : Node option = head  
                let mutable prevNode : Node option = None  
                while currentNode <> None && currentNode <> tail do 
                match currentNode.Value.value.firm.ToString() with 
                | value when value = firmName ->  
                    match prevNode with 
                    | None -> 
                        head <- currentNode.Value.Next 
                        tail.Value.Next <- head
                        currentNode <- tail
                    | Some p -> 
                        p.Next <- currentNode.Value.Next 
                        if currentNode = head then
                            head <- p.Next
                        if currentNode = tail then 
                            tail <- Some(p) 
                        currentNode <- Some(p)
                | _ -> ()
                prevNode <- currentNode
                currentNode <- currentNode.Value.Next
            | _ -> printfn "d"




//let mutable compare = parameters.[0];
            // match parameters.[1] with
            // | "==" -> 
            // | ">=" -> 
            // | "<=" -> 
            // | "<" -> 
            // | ">" -> 
            // | _ -> printfn "Неверный тип продукта"
                // for cur in currentNode.Value.value.GetType().GetProperties() do
                //     match cur.Name with
                //     | value when value = parameters[0] -> 
                //         match parameters.[1] with
                //         | "==" -> if (cur.GetValue(currentNode.Value.value) = parameters[2]) then System.Console.WriteLine(cur.Name)
                //         | ">=" -> if (cur.GetValue(currentNode.Value.value) >= parameters[2]) then System.Console.WriteLine(cur.Name) 
                //         | "<=" -> if (cur.GetValue(currentNode.Value.value) <= parameters[2]) then System.Console.WriteLine(cur.Name) 
                //         | "<" -> if (cur.GetValue(currentNode.Value.value) < parameters[2]) then System.Console.WriteLine(cur.Name) 
                //         | ">" -> if (cur.GetValue(currentNode.Value.value) > parameters[2]) then System.Console.WriteLine(cur.Name)
                //         | _ -> printfn "Неверный тип продукта"
                //     | _ -> ()//System.Console.WriteLine(cur.Name)




                // while currentNode <> None && currentNode <> tail do 
                // match currentNode.Value.value.ToString() with 
                // | value when value = compare -> System.Console.WriteLine(value) 
                //     match prevNode with 
                //     | None -> 
                        // head <- currentNode.Value.Next 
                        // tail.Value.Next <- head
                        // currentNode <- tail
                //     | Some p -> 
                //         p.Next <- currentNode.Value.Next 
                //         if currentNode = head then
                //             head <- p.Next
                //         if currentNode = tail then 
                //             tail <- Some(p) 
                //         currentNode <- Some(p)
                // | _ -> ()
                // prevNode <- currentNode
                // currentNode <- currentNode.Value.Next



 //     while currentNode <> None && currentNode <> tail do  
            //         if currentNode.Value.value.ToString().Contains(firmName) then 
            //             match prevNode with  
            //             | None ->  
            //                 head <- currentNode.Value.Next  
            //                 tail.Value.Next <- head  
            //             | Some p ->  
            //                 p.Next <- currentNode.Value.Next  
            //                 if currentNode = tail then  
            //                     tail <- Some(p) 
            //         prevNode <- currentNode  
            //         currentNode <- currentNode.Value.Next  
            // | _ -> printfn "d"

member this.removeNode (nodeToRemove: Node option) =  
            let mutable currentNode : Node option = head     
            let mutable prevNode : Node option = None  
            while currentNode <> None do  
                if (currentNode = nodeToRemove) then 
                    match prevNode with  
                    | None ->  
                        head <- currentNode.Value.Next  
                        tail.Value.Next <- head
                    | Some p ->  
                        p.Next <- currentNode.Value.Next  
                        if currentNode = head then 
                            head <- p.Next 
                        if currentNode = tail then  
                            tail <- Some(p)
                prevNode <- currentNode 
                currentNode <- currentNode.Value.Next
                if currentNode = head then
                    currentNode <- None











            System.Console.WriteLine(currentNode.Value.value)
            new System.ArgumentException("Цена не может быть отрицательной")