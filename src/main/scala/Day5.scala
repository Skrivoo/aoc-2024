import scala.annotation.tailrec

object Day5 {
  val exampleInput = "47|53\n97|13\n97|61\n97|47\n75|29\n61|13\n75|53\n29|13\n97|29\n53|29\n61|53\n97|53\n61|29\n47|13\n75|47\n97|75\n47|61\n75|61\n47|29\n75|13\n53|13\n\n75,47,61,53,29\n97,61,53,29,13\n75,29,13\n75,97,47,61,53\n61,13,29\n97,13,75,29,47"

  val myInput = "48|17\n38|73\n38|78\n72|63\n72|95\n72|54\n39|56\n39|64\n39|42\n39|57\n22|27\n22|44\n22|64\n22|72\n22|21\n84|14\n84|12\n84|99\n84|81\n84|38\n84|61\n63|64\n63|89\n63|96\n63|73\n63|24\n63|54\n63|71\n74|17\n74|84\n74|42\n74|45\n74|39\n74|54\n74|97\n74|51\n95|14\n95|34\n95|43\n95|17\n95|51\n95|54\n95|94\n95|24\n95|74\n13|61\n13|56\n13|57\n13|38\n13|75\n13|28\n13|21\n13|63\n13|95\n13|64\n12|93\n12|74\n12|34\n12|29\n12|78\n12|19\n12|28\n12|73\n12|75\n12|27\n12|38\n24|39\n24|13\n24|97\n24|99\n24|84\n24|27\n24|61\n24|68\n24|51\n24|22\n24|57\n24|82\n64|96\n64|38\n64|93\n64|43\n64|19\n64|73\n64|54\n64|24\n64|77\n64|48\n64|89\n64|34\n64|71\n28|51\n28|39\n28|27\n28|19\n28|17\n28|93\n28|74\n28|84\n28|45\n28|83\n28|54\n28|69\n28|24\n28|22\n82|19\n82|43\n82|72\n82|12\n82|96\n82|38\n82|28\n82|73\n82|77\n82|74\n82|17\n82|75\n82|14\n82|85\n82|48\n96|19\n96|94\n96|45\n96|69\n96|84\n96|22\n96|27\n96|13\n96|97\n96|24\n96|56\n96|98\n96|17\n96|33\n96|57\n96|71\n89|29\n89|75\n89|19\n89|77\n89|17\n89|93\n89|38\n89|94\n89|24\n89|96\n89|95\n89|71\n89|22\n89|14\n89|28\n89|34\n89|51\n81|63\n81|82\n81|71\n81|73\n81|77\n81|21\n81|95\n81|75\n81|14\n81|74\n81|96\n81|43\n81|38\n81|19\n81|34\n81|48\n81|89\n81|29\n85|19\n85|14\n85|24\n85|48\n85|96\n85|34\n85|73\n85|77\n85|89\n85|28\n85|95\n85|38\n85|22\n85|71\n85|74\n85|12\n85|93\n85|98\n85|54\n99|75\n99|12\n99|17\n99|73\n99|29\n99|21\n99|43\n99|34\n99|71\n99|14\n99|72\n99|85\n99|48\n99|38\n99|77\n99|82\n99|89\n99|28\n99|95\n99|74\n14|24\n14|71\n14|17\n14|38\n14|34\n14|28\n14|94\n14|77\n14|51\n14|48\n14|54\n14|73\n14|75\n14|98\n14|78\n14|93\n14|43\n14|96\n14|27\n14|22\n14|74\n42|82\n42|61\n42|72\n42|95\n42|48\n42|99\n42|68\n42|56\n42|28\n42|57\n42|89\n42|77\n42|21\n42|63\n42|12\n42|43\n42|29\n42|38\n42|34\n42|64\n42|14\n42|85\n19|45\n19|99\n19|98\n19|13\n19|42\n19|54\n19|39\n19|83\n19|27\n19|97\n19|56\n19|24\n19|44\n19|22\n19|61\n19|33\n19|94\n19|51\n19|69\n19|84\n19|93\n19|57\n19|78\n93|57\n93|22\n93|44\n93|98\n93|33\n93|42\n93|39\n93|94\n93|56\n93|99\n93|82\n93|84\n93|45\n93|61\n93|24\n93|69\n93|78\n93|27\n93|81\n93|83\n93|97\n93|68\n93|13\n93|51\n94|22\n94|81\n94|99\n94|57\n94|42\n94|83\n94|68\n94|27\n94|78\n94|39\n94|13\n94|63\n94|51\n94|45\n94|56\n94|44\n94|33\n94|72\n94|61\n94|69\n94|97\n94|21\n94|82\n94|84\n97|82\n97|61\n97|83\n97|57\n97|56\n97|14\n97|64\n97|38\n97|85\n97|13\n97|95\n97|99\n97|42\n97|12\n97|68\n97|72\n97|63\n97|89\n97|21\n97|44\n97|84\n97|69\n97|81\n97|45\n61|14\n61|82\n61|12\n61|63\n61|89\n61|75\n61|74\n61|77\n61|29\n61|68\n61|96\n61|48\n61|34\n61|72\n61|95\n61|85\n61|64\n61|81\n61|21\n61|73\n61|43\n61|99\n61|38\n61|28\n27|56\n27|64\n27|97\n27|69\n27|99\n27|33\n27|45\n27|81\n27|21\n27|72\n27|95\n27|89\n27|61\n27|44\n27|85\n27|82\n27|68\n27|83\n27|57\n27|63\n27|39\n27|84\n27|42\n27|13\n57|34\n57|77\n57|48\n57|74\n57|28\n57|89\n57|12\n57|21\n57|63\n57|82\n57|61\n57|99\n57|85\n57|95\n57|68\n57|72\n57|38\n57|14\n57|29\n57|73\n57|64\n57|75\n57|81\n57|43\n78|56\n78|69\n78|97\n78|33\n78|72\n78|39\n78|99\n78|45\n78|21\n78|13\n78|64\n78|84\n78|61\n78|42\n78|82\n78|81\n78|83\n78|44\n78|63\n78|85\n78|68\n78|27\n78|89\n78|57\n43|45\n43|33\n43|24\n43|74\n43|78\n43|93\n43|22\n43|17\n43|51\n43|34\n43|39\n43|96\n43|27\n43|54\n43|94\n43|73\n43|69\n43|44\n43|19\n43|71\n43|77\n43|97\n43|98\n43|28\n29|43\n29|94\n29|51\n29|71\n29|24\n29|98\n29|44\n29|54\n29|96\n29|45\n29|77\n29|93\n29|74\n29|17\n29|97\n29|73\n29|27\n29|19\n29|33\n29|78\n29|34\n29|28\n29|39\n29|22\n56|72\n56|99\n56|38\n56|14\n56|77\n56|34\n56|85\n56|82\n56|21\n56|64\n56|81\n56|89\n56|75\n56|57\n56|61\n56|12\n56|29\n56|43\n56|95\n56|73\n56|48\n56|28\n56|63\n56|68\n69|57\n69|64\n69|83\n69|82\n69|72\n69|29\n69|63\n69|61\n69|56\n69|48\n69|95\n69|99\n69|89\n69|75\n69|85\n69|68\n69|42\n69|21\n69|84\n69|13\n69|14\n69|12\n69|81\n69|38\n75|94\n75|96\n75|27\n75|98\n75|17\n75|73\n75|77\n75|39\n75|74\n75|71\n75|33\n75|22\n75|28\n75|34\n75|19\n75|29\n75|97\n75|93\n75|44\n75|51\n75|43\n75|24\n75|54\n75|78\n45|85\n45|89\n45|42\n45|48\n45|99\n45|69\n45|75\n45|61\n45|95\n45|14\n45|57\n45|68\n45|81\n45|82\n45|38\n45|13\n45|83\n45|56\n45|72\n45|63\n45|12\n45|64\n45|21\n45|84\n98|21\n98|68\n98|22\n98|61\n98|56\n98|57\n98|94\n98|69\n98|84\n98|44\n98|27\n98|51\n98|45\n98|97\n98|24\n98|82\n98|78\n98|13\n98|81\n98|39\n98|99\n98|42\n98|83\n98|33\n71|84\n71|39\n71|45\n71|69\n71|68\n71|97\n71|57\n71|56\n71|44\n71|93\n71|94\n71|83\n71|61\n71|98\n71|33\n71|54\n71|78\n71|13\n71|22\n71|27\n71|24\n71|42\n71|19\n71|51\n68|21\n68|14\n68|64\n68|43\n68|75\n68|99\n68|89\n68|96\n68|77\n68|85\n68|38\n68|34\n68|29\n68|12\n68|28\n68|48\n68|63\n68|73\n68|95\n68|82\n68|17\n68|74\n68|72\n68|81\n73|97\n73|69\n73|45\n73|22\n73|33\n73|71\n73|39\n73|13\n73|17\n73|19\n73|96\n73|94\n73|54\n73|98\n73|84\n73|44\n73|83\n73|74\n73|27\n73|78\n73|42\n73|93\n73|24\n73|51\n83|75\n83|42\n83|48\n83|43\n83|84\n83|13\n83|12\n83|82\n83|68\n83|63\n83|21\n83|38\n83|29\n83|95\n83|85\n83|56\n83|61\n83|99\n83|64\n83|81\n83|89\n83|14\n83|72\n83|57\n54|33\n54|24\n54|69\n54|93\n54|13\n54|84\n54|99\n54|68\n54|42\n54|97\n54|83\n54|22\n54|98\n54|45\n54|51\n54|81\n54|78\n54|61\n54|27\n54|94\n54|56\n54|39\n54|57\n54|44\n33|44\n33|81\n33|82\n33|89\n33|42\n33|72\n33|12\n33|84\n33|61\n33|63\n33|69\n33|45\n33|64\n33|97\n33|57\n33|99\n33|85\n33|83\n33|39\n33|21\n33|13\n33|68\n33|95\n33|56\n77|39\n77|27\n77|73\n77|54\n77|96\n77|24\n77|34\n77|98\n77|97\n77|44\n77|78\n77|69\n77|93\n77|51\n77|33\n77|94\n77|83\n77|28\n77|71\n77|19\n77|22\n77|45\n77|74\n77|17\n51|57\n51|69\n51|33\n51|27\n51|97\n51|56\n51|85\n51|78\n51|45\n51|42\n51|61\n51|44\n51|81\n51|39\n51|72\n51|13\n51|64\n51|21\n51|99\n51|83\n51|82\n51|68\n51|84\n51|63\n17|45\n17|39\n17|19\n17|69\n17|61\n17|78\n17|98\n17|56\n17|94\n17|51\n17|93\n17|24\n17|33\n17|42\n17|44\n17|22\n17|84\n17|27\n17|13\n17|54\n17|71\n17|83\n17|57\n17|97\n34|96\n34|71\n34|84\n34|39\n34|51\n34|97\n34|13\n34|17\n34|45\n34|83\n34|27\n34|78\n34|24\n34|33\n34|74\n34|19\n34|93\n34|69\n34|44\n34|98\n34|22\n34|54\n34|73\n34|94\n44|63\n44|42\n44|83\n44|64\n44|81\n44|85\n44|57\n44|95\n44|89\n44|48\n44|61\n44|84\n44|13\n44|21\n44|56\n44|12\n44|38\n44|69\n44|72\n44|68\n44|14\n44|82\n44|45\n44|99\n21|71\n21|75\n21|63\n21|19\n21|34\n21|74\n21|73\n21|89\n21|54\n21|96\n21|93\n21|77\n21|95\n21|29\n21|14\n21|72\n21|64\n21|12\n21|48\n21|38\n21|17\n21|43\n21|85\n21|28\n48|97\n48|39\n48|73\n48|54\n48|28\n48|33\n48|74\n48|94\n48|51\n48|43\n48|29\n48|93\n48|27\n48|98\n48|78\n48|34\n48|71\n48|22\n48|77\n48|24\n48|75\n48|96\n48|19\n38|77\n38|93\n38|94\n38|48\n38|29\n38|19\n38|33\n38|54\n38|17\n38|28\n38|75\n38|39\n38|71\n38|27\n38|22\n38|96\n38|98\n38|51\n38|43\n38|74\n38|24\n38|34\n72|29\n72|38\n72|14\n72|19\n72|71\n72|43\n72|89\n72|28\n72|64\n72|77\n72|34\n72|85\n72|75\n72|12\n72|48\n72|17\n72|96\n72|93\n72|74\n72|73\n72|98\n39|89\n39|12\n39|21\n39|84\n39|63\n39|44\n39|45\n39|85\n39|69\n39|72\n39|13\n39|83\n39|61\n39|14\n39|81\n39|95\n39|82\n39|68\n39|99\n39|97\n22|51\n22|99\n22|69\n22|39\n22|33\n22|82\n22|61\n22|13\n22|42\n22|45\n22|97\n22|78\n22|83\n22|56\n22|57\n22|84\n22|81\n22|68\n22|63\n84|77\n84|63\n84|72\n84|43\n84|85\n84|29\n84|48\n84|56\n84|82\n84|42\n84|13\n84|89\n84|21\n84|95\n84|68\n84|64\n84|75\n84|57\n63|12\n63|38\n63|34\n63|14\n63|93\n63|95\n63|17\n63|48\n63|29\n63|43\n63|77\n63|28\n63|19\n63|98\n63|74\n63|85\n63|75\n74|96\n74|22\n74|98\n74|93\n74|19\n74|24\n74|83\n74|56\n74|69\n74|78\n74|27\n74|13\n74|71\n74|44\n74|94\n74|33\n95|93\n95|75\n95|73\n95|29\n95|19\n95|48\n95|71\n95|38\n95|77\n95|78\n95|28\n95|12\n95|98\n95|22\n95|96\n13|43\n13|12\n13|85\n13|82\n13|48\n13|89\n13|81\n13|14\n13|68\n13|99\n13|42\n13|72\n13|29\n13|77\n12|51\n12|98\n12|54\n12|22\n12|24\n12|94\n12|14\n12|17\n12|96\n12|77\n12|71\n12|48\n12|43\n24|94\n24|44\n24|56\n24|72\n24|33\n24|78\n24|69\n24|21\n24|42\n24|83\n24|81\n24|45\n64|28\n64|95\n64|85\n64|29\n64|75\n64|14\n64|17\n64|94\n64|12\n64|98\n64|74\n28|73\n28|71\n28|34\n28|33\n28|96\n28|78\n28|97\n28|98\n28|94\n28|44\n82|21\n82|89\n82|54\n82|95\n82|64\n82|71\n82|34\n82|63\n82|29\n96|78\n96|54\n96|51\n96|93\n96|42\n96|44\n96|39\n96|83\n89|98\n89|74\n89|48\n89|12\n89|73\n89|54\n89|43\n81|28\n81|72\n81|64\n81|12\n81|17\n81|85\n85|29\n85|94\n85|75\n85|43\n85|17\n99|96\n99|63\n99|64\n99|81\n14|29\n14|19\n14|33\n42|81\n42|75\n19|68\n\n51,78,33,39,97,44,45,69,83,84,13,42,56,57,61,68,99,81,82,21,72,63,64\n78,34,43,71,33,77,93,22,17,74,73,75,97\n99,98,27,57,84,61,39,51,24,81,33\n96,98,51,27,97,13,42\n99,81,82,72,63,89,12,14,38,75,29,43,28,34,73,96,17\n96,71,54,93,98,24,22,78,27,33,97,69,83,84,13,42,56\n94,22,78,97,45,69,56,57,99\n34,73,74,96,17,19,54,93,98,24,22,78,27,39,97,45,69,83,84\n93,98,22,51,78,39,97,45,69,84,13,56,57,61,68,99,81\n77,28,17,71,19,93,22,51,78,27,39,97,45\n12,14,75,29,43,77,73,74,96,71,19,54,93,94,22,51,78\n28,73,98,22,51,44,83\n95,12,14,48,75,43,77,96,17,71,54,98,24,94,51\n96,71,19,54,93,24,94,22,27,33,39,97,69,83,84,42,56\n24,94,22,51,78,27,33,39,97,44,45,69,83,84,13,42,56,57,68,99,81,82,21\n21,72,64,85,89,12,14,38,48,29,43,28,34,17,71,19,54\n13,56,33,78,96,44,94\n28,98,39,51,83,78,97,24,54,93,69,94,73,71,27,34,74\n99,81,82,72,64,85,89,12,14,48,75,77,34,74,17\n56,57,61,68,99,81,82,21,72,63,64,95,12,14,38,48,75,29,43,28,34\n73,96,71,19,54,93,98,24,94,22,51,27,33,39,97,44,45,69,83,84,13\n27,39,84,68,72\n82,72,61,69,38,64,63,45,42,83,48,85,99,13,89\n83,84,13,56,57,61,68,99,81,82,21,72,64,85,89,12,38,48,29\n19,93,27,33,42,57,68\n57,72,81,21,27,97,13,84,78,68,85,63,83,61,45\n38,48,96,17,19\n13,42,56,57,61,68,81,82,21,72,63,64,85,89,95,12,14,38,48,75,29,43,77\n13,56,21,95,75,29,77\n34,93,78,48,22,98,71,29,39,24,17\n42,57,61,68,82,21,72,63,64,85,95,12,14,38,48,75,29,77,28\n68,27,99,69,85,61,64,97,56\n77,75,24,34,19,17,71,94,43,48,73,28,98,96,33,51,78,93,29,39,27,74,22\n93,75,95,63,98,74,96\n61,78,94,99,97,82,42,98,24\n54,93,94,27,33,45,69,83,42,61,99\n77,28,96,71,54,93,24,94,22,51,78,33,44,45,69\n89,95,12,14,38,48,75,29,77,28,34,73,74,96,17,71,19,54,93,98,24,94,22\n38,29,96,24,94\n43,77,28,73,74,19,54,24,94,51,78,27,39,97,45\n44,45,83,13,42,56,57,68,99,81,82,21,63,64,85,89,95,14,38\n38,43,73,71,54,51,33\n14,38,48,75,77,54,27\n51,78,33,97,69,83,13,57,61,68,81,82,21,63,64\n34,74,96,17,71,19,54,93,24,94,22,27,33,39,45,83,84\n33,57,51,98,82,81,97,44,22\n54,94,51,78,27,33,45,83,13,61,99\n22,19,38,43,73,28,77,78,75,29,96,98,17,54,94,48,74\n33,61,69,42,71,97,98,45,13\n29,43,77,28,34,73,74,96,17,71,19,93,98,24,94,22,51,78,33,97,44\n28,77,99,29,14,21,72,75,43,48,64,42,57,89,81,85,82\n85,74,89,34,71,48,63,19,82,28,12,17,96,14,75,64,43,38,21,77,29\n99,12,83,61,68,82,13,81,56,63,89,42,85,21,97,57,64,44,95,69,84,45,14\n45,61,69,99,33,56,68\n69,99,13,63,89,38,64,21,12,42,84,57,83,68,14,81,95,61,45\n45,74,98,24,84,94,39,69,51,19,83,44,54,96,27,78,17,33,42,93,22\n77,28,73,74,17,71,54,98,24,22,51,78,27,33,39,97,44,45,69\n61,68,99,81,82,21,72,63,85,89,95,12,14,38,48,29,43,77,28,34,74\n43,42,61,72,38,68,99,29,81,12,84\n54,93,98,24,94,22,78,39,97,45,69,56,57,68,99\n64,85,12,14,48,75,43,77,28,34,74,96,17,19,93,98,24\n73,17,54,93,33,39,97\n14,38,48,75,29,77,28,34,73,96,17,19,54,24,94,22,51,78,27\n82,21,72,64,85,89,95,12,14,38,48,75,29,43,28,34,17,71,19\n93,17,34,72,48,38,14,75,74,85,71,19,28,96,95\n95,14,38,48,75,29,43,28,73,17,54,98,24,94,51\n73,77,19,78,75,24,94,39,96,48,27\n96,17,71,19,54,93,98,24,94,22,51,78,27,33,39,97,44,45,69,83,84,42,56\n19,22,33,97,44,45,69,42,56,57,68\n29,43,77,28,34,73,17,71,19,54,98,24,94,22,51,78,27,33,39,97,44\n72,89,95,38,43,28,34,17,93\n74,96,71,19,54,93,98,24,94,22,51,78,27,39,44,45,69\n27,97,84,68,82,21,63,85,89\n69,84,13,72,64,12,38\n28,17,89,81,99,63,74,73,75,77,43,29,14,96,95\n82,21,72,63,64,85,89,12,14,29,43,28,34,73,74,96,17,71,19\n27,73,39,71,96,78,51,54,74,97,22\n39,13,21,27,81,56,89,72,68,85,69,63,84,99,83,44,33\n29,43,77,34,74,96,17,71,19,54,93,98,24,22,51,78,27,33,39,97,44\n84,24,56,51,27,93,97,13,22,94,98,78,83,81,33,69,44\n13,56,57,68,99,82,72,63,64,85,89,95,12,14,38,48,75\n33,97,45,69,83,84,13,42,56,68,99,81,82,21,72,63,85,89,95\n71,93,24,94,97,83,61\n57,61,68,99,81,21,63,85,95,12,38,48,29\n78,27,33,97,44,45,69,83,84,56,57,68,82,21,85\n74,21,81,63,12,75,89,95,96,64,17\n34,93,28,94,19,73,97,43,75,17,54,27,22,98,51,39,77,96,78,71,33,24,29\n89,95,12,38,75,29,28,73,71,19,54,24,94\n95,74,48,73,14,77,28,17,85,89,63,21,82,81,43,75,29,34,38\n27,33,61,68,99,81,21,63,85\n96,17,71,19,54,93,98,24,94,22,51,27,33,39,97,44,45,69,83,84,13,42,56\n89,95,12,38,75,29,43,77,28,34,74,96,17,71,19,93,98,24,22\n44,98,84,61,51,69,45,57,78\n57,68,72,89,75,77,73\n77,24,12,17,54,93,85,43,29\n97,45,44,72,84,21,81,63,68,64,78,39,51,56,57,42,61\n38,48,43,74,17,19,78\n69,42,68,99,72,14,75\n54,93,98,78,27,33,97,44,45,69,84,56,61,68,99\n12,42,38,82,81,89,45,56,21,85,95,64,83,99,13,61,14,44,63\n99,83,82,21,68,14,63,75,42,89,61,12,56,95,57,29,64,85,84,48,38\n99,73,48,89,95,74,68,77,29,21,72,12,64,34,96\n82,21,72,85,12,48,29,28,34,96,19\n56,57,61,68,99,81,82,21,72,63,64,85,89,95,12,14,38,75,29,43,77,28,34\n34,63,43,72,89,95,54,93,85,64,73,17,19,96,38\n54,24,22,51,97\n29,43,94,28,24,34,19,74,95,38,77,89,48,14,75\n84,78,63,83,82,68,39,99,81,57,72,69,56,27,22\n42,57,48,14,95\n33,13,57,61,81\n83,57,27,33,64,69,61,72,39,68,42,97,89,85,82,56,99,84,45,44,13\n78,69,94,98,22,17,97,39,84,24,34,51,83,54,33\n96,71,19,93,98,24,94,22,78\n71,19,54,93,98,94,22,27,33,39,44,45,83,84,13,42,56,57,61\n93,98,29,43,19,22,96,71,24,73,74\n75,43,77,34,17,71,19,54,98,94,22,27,97\n54,98,24,94,97,69,84,57,61,68,99\n43,77,34,74,96,71,54,98,24,94,22,51,78,27,33,39,45\n77,34,96,17,71,54,93,94,22,27,33,39,69\n22,33,97,44,45,83,13,42,56,57,61,68,99,81,63\n42,57,61,68,99,81,82,21,72,63,64,95,12,38,48,29,43,77,28\n57,21,64,89,14,75,29\n61,81,82,21,72,64,89,14,48,29,43,77,74\n85,89,81,75,38,13,48,83,14,29,82\n45,69,83,84,13,42,56,57,61,99,82,21,64,85,89,12,14,38,48\n28,34,73,96,17,71,19,54,98,94,22,78,27,39,97,44,45,69,83\n71,38,17,29,96,95,19,85,48,74,93,54,28,34,14,94,43\n75,77,28,34,73,74,96,19,54,98,24,51,78,27,33,39,97\n72,85,95,68,57,75,84,64,42,29,81,89,14,63,48,56,82,12,83,61,38,13,99\n51,13,21,81,84\n33,39,97,44,69,83,13,42,56,57,61,68,21,72,64,85,95\n71,24,97,83,42\n42,63,75,95,77,21,85,56,13\n89,95,14,29,43,28,74,96,17,71,54,93,98\n33,39,97,44,69,84,13,42,56,57,61,82,72,63,64,89,95\n57,61,68,99,81,82,21,72,63,64,85,89,95,12,14,38,48,75,29,77,28,34,73\n68,82,21,72,85,89,95,12,14,38,48,29,28,73,96\n12,38,29,28,96,19,93\n29,43,28,34,73,74,54,93,98,24,94,51,78,27,33,39,44\n56,68,21,63,89,12,48,75,34\n77,12,73,93,29,95,89,28,72,19,71,63,96,34,74,38,75,54,64\n78,33,19,51,73,22,39,93,74,84,13\n93,51,83,57,39,17,71,22,24\n98,73,71,48,17,63,64,29,93,96,89\n21,13,69,44,94,84,45,82,56,72,57,61,99\n48,77,28,34,94,22,39\n39,69,22,81,24,56,44,13,94,98,83,33,51,68,97,82,61,42,45\n43,77,28,34,73,74,96,17,71,19,54,93,98,24,22,51,78,27,33,39,97,44,45\n17,54,93,94,22,51,78,27,33,97,84,56,57\n97,42,82,63,95\n34,68,61,72,38,85,29,77,43,75,12,99,89,28,64,82,73,81,74,95,48,14,63\n64,29,72,75,43,38,13,84,99\n93,28,77,43,48,38,74,71,73,34,27,96,24,14,29,51,17,22,54,75,19,98,78\n45,39,56,95,21,84,99,97,72,83,63,85,42,44,61,64,68,13,81,89,57,12,69\n63,85,19,71,43,29,77,93,54,14,95,73,64,74,12,28,96,34,98\n27,33,69,21,84,72,99,63,13,57,89,44,83,82,42\n94,78,71,51,22,93,44,19,96,39,54,27,74,73,98,34,69,28,45,83,33,24,97\n45,83,42,68,81,21,72,64,85,95,12,14,48\n69,57,22,44,99,33,27,39,51,81,13,97,56,61,68,83,42,94,82,84,24\n42,56,57,68,21,72,64,14,48,43,28\n84,13,56,57,68,82,21,72,12,75,43\n73,74,96,17,19,54,93,98,24,22,78,27,33,39,97,44,45,69,83,84,13\n39,97,44,84,42,56,57,61,81,72,64,85,89,95,12\n19,64,43,54,63,75,71,38,93,73,72,14,28,17,34,48,95\n28,95,74,72,14,34,81\n73,96,17,71,19,54,93,98,24,94,22,51,78,27,33,39,97,44,45,69,83,84,13\n42,83,63,21,89,38,68,75,69,64,82\n96,51,27,93,98,39,44,13,45\n28,14,72,12,21,82,63,56,57,89,77,43,38,61,64,68,85,48,34,81,95\n97,45,83,13,56,61,81,72,63,64,89,12,14\n64,85,89,95,12,14,48,43,28,34,73,74,96,71,54,98,24\n61,69,42,63,78\n56,57,81,89,14,38,48,75,34\n22,51,78,27,33,39,97,44,45,69,83,84,13,42,56,57,61,68,99,82,21,72,63\n89,38,34,73,48,29,93,28,17,95,77,74,72\n75,29,43,77,28,34,73,74,96,17,71,19,93,98,24,94,22,51,78,27,33,39,97\n74,96,17,71,54,98,24,22,51,27,33,39,45,69,84,13,42\n75,28,73,74,54,93,24,94,78,27,33,39,97\n81,72,63,64,85,89,12,48,75,29,43,77,28,34,73,74,96,17,71\n97,44,45,69,83,84,42,56,57,61,68,99,81,82,21,72,63,64,85,89,95,12,14\n71,28,77,75,96,63,81,85,21\n83,42,39,61,45,97,27,84,72\n95,85,77,96,73,89,71\n57,68,99,82,21,72,64,85,95,14,48,75,29,43,77,28,73\n21,64,85,89,38,75,29,43,28,73,74,17,71,19,54\n48,75,29,43,28,34,74,71,19,54,24,22,78\n82,21,72,63,64,85,89,95,12,14,38,48,75,29,43,77,28,34,73,74,96,71,19\n69,22,94,39,33,83,44,24,13,54,93,45,73"

  case class OrderingRule(n: Int, m: Int)
  private object OrderingRule {
    def apply(input: String): OrderingRule = {
      val a = input.split('|')
      OrderingRule(a(0).toInt, a(1).toInt)
    }
  }
  case class Update(list: List[Int])
  private object Update {
    def apply(input: String): Update = {
      Update(input.split(',').map(_.toInt).toList)
    }
  }
  private case class Print(orderingRuleList: List[OrderingRule], updateList: List[Update])

  private val print: Print = {
    val splitInput = myInput.split("\n\n")
    val orderingRuleList = splitInput(0).split("\n").map(OrderingRule(_)).toList
    val updateList = splitInput(1).split("\n").map(Update(_)).toList
    Print(orderingRuleList, updateList)
  }

  def day5task1: Int = {
    val validUpdates = getValidUpdates(print, List.empty)
    validUpdates.map(e => e.list(e.list.length / 2)).sum
  }

  @tailrec
  private def getValidUpdates(print: Print, validUpdates: List[Update]): List[Update] = {
    print.updateList.headOption match
      case Some(update) if isValidUpdate(update, print.orderingRuleList) => getValidUpdates(Print(print.orderingRuleList, print.updateList.tail), validUpdates :+ update)
      case Some(update) => getValidUpdates(Print(print.orderingRuleList, print.updateList.tail), validUpdates)
      case None => validUpdates
  }

  @tailrec
  private def isValidUpdate(update: Update, orderingRuleList: List[OrderingRule]): Boolean = {
    update.list.headOption match
      case Some(value) => {
        val theRestOfListValues = update.list.tail.toSet
        val rulesWhichMaybeBreak = orderingRuleList.filter(e => theRestOfListValues.contains(e.n))
        if (rulesWhichMaybeBreak.exists(_.m == value)) {
          false
        } else {
          isValidUpdate(Update(update.list.tail), orderingRuleList)
        }
      }
      case None => true
  }
}
