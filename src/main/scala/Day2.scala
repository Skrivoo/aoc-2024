import scala.annotation.tailrec

object Day2 {

  private val exampleInput = "7 6 4 2 1\n1 2 7 8 9\n9 7 6 2 1\n1 3 2 4 5\n8 6 4 4 1\n1 3 6 7 9"
  private val myInput = "6 8 9 11 10\n13 16 19 22 24 25 25\n43 46 47 48 50 52 56\n55 57 60 61 64 65 68 74\n81 84 86 83 84 87\n45 47 50 49 46\n15 16 18 16 16\n79 80 81 79 80 83 85 89\n70 73 74 72 77\n78 80 81 81 83\n77 79 82 85 88 88 89 87\n74 75 75 78 80 82 82\n83 84 84 87 88 90 94\n9 11 14 16 16 21\n40 42 44 48 51 53 56\n71 74 78 81 80\n7 9 11 14 18 20 21 21\n74 77 81 82 83 87\n76 77 79 80 84 86 93\n62 63 69 72 75 77 78\n21 23 29 32 35 37 35\n85 87 88 94 94\n45 47 49 56 60\n45 46 51 54 56 61\n63 60 62 65 67 69\n39 37 40 43 44 45 46 45\n84 81 84 87 87\n93 91 93 95 99\n45 44 46 47 49 56\n72 69 68 69 72\n83 81 84 81 80\n4 3 5 7 6 8 10 10\n92 90 87 89 93\n46 45 47 44 47 48 49 54\n43 42 43 43 45\n18 15 15 17 20 21 19\n72 71 72 73 75 75 78 78\n49 46 47 49 51 51 52 56\n73 72 73 74 75 78 78 84\n52 49 53 55 57 60 62\n21 19 23 25 27 29 28\n80 77 78 81 85 86 88 88\n21 20 24 27 31\n37 35 39 42 48\n64 61 63 69 70 72 74 75\n84 83 85 88 94 91\n24 23 30 32 34 37 37\n70 69 70 75 79\n4 3 8 10 13 20\n13 13 15 17 18 19 21\n9 9 10 12 15 13\n50 50 53 55 57 60 62 62\n5 5 7 9 13\n43 43 45 47 48 50 55\n79 79 81 84 82 85 88\n92 92 95 93 96 95\n22 22 23 22 23 25 26 26\n84 84 85 83 87\n72 72 70 71 72 78\n17 17 19 19 20 21 24\n41 41 44 47 47 44\n31 31 31 32 34 35 35\n7 7 9 12 12 14 18\n21 21 21 22 29\n83 83 85 86 88 92 94 97\n26 26 27 30 33 37 35\n28 28 32 35 35\n82 82 85 88 92 96\n6 6 10 12 18\n83 83 88 89 90 92 95\n19 19 24 26 28 27\n41 41 48 49 49\n25 25 32 33 34 36 40\n28 28 29 30 33 36 43 50\n83 87 89 90 93 96 99\n46 50 53 54 55 58 59 56\n65 69 70 73 76 78 78\n67 71 72 75 77 78 82\n82 86 87 89 96\n81 85 86 88 91 93 92 94\n59 63 66 67 65 67 64\n16 20 19 20 20\n89 93 96 93 97\n70 74 72 74 77 83\n7 11 13 14 14 15 16\n26 30 31 33 36 39 39 38\n55 59 62 64 64 66 66\n30 34 35 35 38 40 43 47\n5 9 9 12 15 22\n43 47 49 52 53 54 58 61\n38 42 46 48 51 53 50\n1 5 6 10 10\n9 13 17 20 21 23 27\n28 32 36 39 40 43 48\n23 27 33 36 38 40 41\n51 55 57 63 61\n64 68 70 77 78 81 83 83\n22 26 29 32 37 41\n80 84 90 92 93 99\n57 63 66 68 70\n75 82 84 85 86 85\n52 58 61 62 62\n49 56 59 61 63 66 70\n33 39 40 42 45 46 53\n71 77 75 76 77\n73 79 76 79 82 84 82\n53 58 60 62 64 63 63\n47 54 51 53 55 58 60 64\n70 77 74 76 83\n41 46 46 48 49\n26 31 33 33 32\n82 89 91 91 92 92\n54 60 60 61 65\n20 25 26 26 31\n49 56 57 61 62 65\n60 65 68 70 72 76 77 75\n8 13 16 20 22 24 27 27\n17 24 25 29 31 35\n74 79 83 86 93\n5 11 14 21 22 25 28\n48 53 55 61 64 67 69 68\n68 73 74 80 82 82\n9 14 19 20 21 25\n10 15 22 24 29\n68 65 63 61 59 60\n25 24 21 20 20\n71 69 66 65 62 58\n77 76 73 71 64\n83 80 81 79 78 75\n20 17 20 19 18 20\n70 67 65 64 66 66\n93 92 94 92 88\n75 72 69 70 68 61\n12 10 7 7 5 4 2\n63 62 59 59 62\n18 17 14 13 12 12 10 10\n82 80 78 78 75 71\n26 24 23 23 18\n49 47 43 42 39 36 34 33\n50 49 46 42 39 41\n38 35 31 29 27 25 25\n15 14 10 7 5 1\n69 66 64 60 59 56 55 49\n77 75 73 67 64 62\n95 93 90 87 81 82\n14 13 12 10 9 4 4\n62 59 56 49 46 42\n47 44 37 34 32 30 29 24\n79 81 78 75 72 70 67 65\n62 64 63 60 58 59\n66 67 66 64 63 61 61\n38 39 38 36 34 30\n39 42 41 39 38 37 34 27\n58 61 58 55 53 52 53 52\n82 85 87 85 88\n25 26 27 25 23 21 21\n87 89 91 88 87 86 84 80\n34 35 33 34 32 27\n85 88 85 84 84 83\n14 16 16 14 15\n16 18 15 12 12 10 10\n55 57 54 52 52 48\n71 74 73 73 72 67\n25 28 24 21 19 16\n59 62 60 56 55 53 52 54\n55 57 53 50 47 45 44 44\n34 37 34 33 32 28 24\n84 87 83 80 78 76 73 66\n23 25 23 17 15\n75 78 76 74 73 70 65 68\n24 25 19 17 17\n95 98 92 91 87\n91 92 90 83 77\n45 45 44 43 42\n88 88 85 82 79 78 75 77\n55 55 54 53 50 50\n73 73 70 67 64 60\n97 97 94 92 91 84\n7 7 6 3 4 3\n71 71 70 67 65 66 67\n69 69 70 67 64 61 60 60\n20 20 18 16 14 16 15 11\n39 39 36 38 35 33 30 25\n42 42 42 40 38 36\n49 49 46 46 44 43 45\n19 19 17 15 15 13 13\n81 81 81 78 76 75 71\n20 20 17 17 14 12 11 5\n74 74 73 69 68 66 63\n68 68 65 61 58 61\n44 44 40 37 34 34\n60 60 59 55 53 49\n38 38 36 32 30 23\n70 70 68 62 61 58\n51 51 49 48 41 39 41\n28 28 25 19 18 18\n19 19 12 9 8 4\n83 83 76 73 67\n66 62 59 56 55 53\n19 15 14 13 15\n78 74 72 69 67 66 66\n93 89 88 87 84 82 78\n92 88 87 86 85 83 80 73\n84 80 82 80 78 76\n71 67 69 66 67\n84 80 78 81 81\n25 21 18 17 16 18 14\n57 53 55 54 47\n18 14 14 11 10\n66 62 59 58 55 55 58\n79 75 74 74 74\n67 63 60 60 59 56 52\n49 45 42 40 38 37 37 31\n94 90 87 85 81 79\n30 26 23 22 21 17 19\n75 71 68 65 63 60 56 56\n97 93 91 90 86 83 79\n23 19 18 14 12 6\n93 89 88 82 79 77 74\n57 53 52 50 47 42 45\n41 37 34 32 27 27\n96 92 91 86 84 81 80 76\n57 53 47 44 41 39 33\n49 43 41 39 38 37 35\n44 38 37 35 38\n24 18 16 13 11 11\n67 61 58 57 55 51\n60 55 53 52 46\n49 44 46 44 41\n16 9 10 8 5 7\n27 21 20 21 19 16 16\n20 15 13 16 13 9\n91 84 87 86 81\n60 55 52 49 49 46 45\n16 9 7 5 4 4 3 5\n94 88 88 86 83 83\n84 77 76 76 73 70 68 64\n65 59 57 55 52 49 49 42\n47 41 38 36 32 30\n24 19 18 14 12 14\n43 38 36 32 32\n78 71 68 64 62 58\n38 31 27 26 20\n30 24 23 18 16 13\n61 55 52 45 42 39 37 40\n30 24 23 22 17 15 15\n75 69 64 62 60 57 55 51\n94 89 86 80 78 71\n13 15 18 21 19\n20 22 23 26 29 29\n41 43 46 49 53\n27 30 31 32 39\n18 21 23 22 23 26 28\n64 66 69 66 68 69 71 68\n15 18 21 19 21 24 24\n76 78 80 77 81\n60 61 62 64 63 64 70\n13 16 17 17 20 21\n66 69 72 72 75 74\n14 16 16 17 17\n5 6 6 8 12\n1 3 6 6 12\n64 67 71 74 75 78 81 83\n20 22 24 26 30 33 35 34\n29 31 35 37 37\n31 32 36 37 40 42 46\n59 62 65 69 72 74 77 82\n77 78 83 84 85 88 91\n72 74 76 82 83 85 83\n12 13 15 21 21\n63 65 68 70 76 80\n42 44 47 48 53 54 55 60\n43 41 43 45 46 49\n38 36 39 40 41 40\n16 15 16 17 17\n29 27 29 32 35 39\n28 25 27 30 33 38\n44 41 43 40 42 44 46\n72 70 71 70 72 70\n83 80 78 81 83 85 88 88\n43 41 43 45 46 49 47 51\n69 66 65 66 67 68 74\n31 28 31 31 34\n66 64 67 70 70 71 68\n86 83 83 86 87 89 89\n76 74 74 76 79 82 86\n6 3 3 4 10\n43 42 43 44 47 49 53 55\n64 62 64 68 65\n61 59 62 64 65 69 72 72\n87 85 88 92 96\n48 46 50 53 60\n12 10 15 17 19 20\n26 25 27 29 30 37 34\n25 22 25 27 32 33 35 35\n55 53 55 62 65 67 70 74\n79 78 80 87 89 96\n58 58 61 62 64 66 68\n66 66 68 71 73 75 72\n30 30 32 33 34 37 37\n30 30 33 36 38 40 44\n78 78 80 81 83 90\n56 56 59 62 65 63 64 66\n38 38 39 40 37 39 41 40\n79 79 77 79 82 84 87 87\n21 21 19 21 22 24 28\n23 23 22 25 31\n68 68 70 72 72 75\n21 21 22 22 23 25 23\n22 22 25 26 26 28 28\n84 84 84 87 90 92 96\n76 76 79 79 85\n26 26 28 32 34 36 38 40\n15 15 17 21 19\n59 59 62 65 69 71 71\n30 30 32 35 38 42 43 47\n53 53 54 55 59 64\n28 28 30 37 38\n57 57 64 67 66\n65 65 66 69 74 74\n49 49 55 56 60\n39 39 41 42 49 56\n77 81 83 84 86 89 91\n12 16 18 19 22 24 22\n24 28 30 33 35 38 41 41\n85 89 91 93 95 99\n1 5 6 9 11 18\n66 70 72 74 75 77 75 76\n67 71 72 74 76 73 70\n2 6 8 11 12 9 9\n52 56 53 55 56 60\n15 19 22 21 28\n71 75 77 77 80 83\n16 20 22 22 25 23\n18 22 25 26 26 26\n8 12 12 14 15 18 19 23\n53 57 60 63 63 64 66 72\n37 41 44 48 50\n32 36 38 42 44 47 46\n3 7 9 11 13 17 17\n72 76 80 83 87\n61 65 68 70 72 76 77 82\n35 39 41 48 51 53\n80 84 85 92 94 95 94\n17 21 24 31 31\n61 65 70 73 76 80\n34 38 41 44 50 51 58\n22 27 30 31 33 36 39 42\n51 58 59 60 63 61\n35 41 42 44 45 46 49 49\n77 83 85 87 89 93\n14 20 23 24 25 27 32\n28 34 36 37 39 36 38\n62 69 71 70 68\n79 84 81 82 85 86 86\n63 69 71 68 70 74\n18 24 21 22 23 28\n76 81 82 83 83 84 86 87\n62 69 72 72 69\n79 84 84 87 90 90\n4 10 11 11 15\n46 52 55 56 56 58 63\n58 65 68 72 73\n44 50 53 55 59 56\n67 72 76 79 81 83 84 84\n64 70 72 76 77 79 80 84\n14 21 25 27 29 35\n11 16 18 19 25 27 30\n26 32 34 41 39\n18 25 26 29 35 35\n15 22 28 29 32 33 37\n74 81 82 88 95\n56 53 52 51 50 48 51\n11 9 6 3 3\n42 39 36 35 31\n67 64 62 60 54\n9 6 9 8 5\n38 35 33 36 35 36\n59 57 56 53 55 55\n97 96 94 92 93 89\n88 87 86 87 86 81\n40 37 37 34 31\n29 28 28 27 26 29\n91 89 87 84 84 84\n85 84 84 81 77\n16 14 11 11 5\n95 94 91 90 86 83\n40 39 38 37 33 35\n32 31 27 26 26\n22 20 16 13 9\n32 29 27 25 24 20 19 14\n60 57 55 50 48\n28 25 22 20 13 10 8 9\n45 43 42 36 33 30 30\n89 88 86 79 77 76 73 69\n27 26 19 16 13 10 3\n21 24 23 21 20 19 18\n32 35 32 30 28 27 29\n68 71 68 65 63 60 59 59\n58 59 58 57 53\n43 46 44 43 40 38 35 28\n9 11 12 10 7\n85 86 83 80 77 80 77 78\n8 10 8 7 5 6 6\n53 56 53 52 53 49\n24 27 25 23 20 22 20 15\n12 15 14 14 13 10\n53 54 53 50 50 47 44 45\n36 38 35 35 34 33 31 31\n36 39 39 37 33\n71 72 72 71 69 64\n11 13 11 10 6 4\n52 53 49 48 50\n59 61 59 55 54 54\n82 84 81 77 76 73 71 67\n56 57 54 53 49 46 44 37\n64 66 65 60 57\n65 67 64 58 57 56 59\n90 92 85 84 81 78 78\n43 45 44 42 36 32\n27 29 26 24 23 20 14 9\n53 53 51 49 47 44 42 39\n85 85 82 81 79 76 75 78\n51 51 50 48 46 46\n78 78 75 73 71 69 67 63\n36 36 33 31 29 22\n73 73 71 74 71\n16 16 13 11 13 14\n64 64 61 62 62\n53 53 55 52 48\n48 48 49 48 41\n28 28 26 26 25\n48 48 46 43 43 42 45\n77 77 74 72 71 71 68 68\n36 36 33 31 31 30 28 24\n26 26 23 23 17\n89 89 86 82 79 76 75\n14 14 13 12 8 6 8\n88 88 84 81 80 80\n55 55 54 50 47 43\n99 99 95 92 89 86 84 78\n56 56 54 47 44\n18 18 16 14 8 7 8\n87 87 80 79 78 76 74 74\n86 86 81 78 76 72\n87 87 84 77 75 73 66\n82 78 76 73 70 69 68\n38 34 32 30 27 25 24 25\n64 60 59 56 55 55\n50 46 45 44 42 38\n21 17 14 11 9 3\n39 35 34 32 34 31\n73 69 67 64 61 58 59 62\n23 19 16 14 17 17\n22 18 19 17 13\n27 23 21 19 21 18 17 12\n60 56 55 53 50 50 49\n21 17 14 14 17\n61 57 57 55 53 52 52\n89 85 83 82 82 81 79 75\n90 86 84 84 78\n52 48 46 42 39\n85 81 79 78 74 72 69 72\n40 36 34 31 28 24 24\n89 85 81 79 76 74 70\n43 39 35 32 29 27 22\n45 41 35 34 33 32 30 28\n74 70 68 62 64\n92 88 86 81 79 76 73 73\n71 67 64 63 57 56 52\n93 89 88 81 80 75\n27 21 18 16 14\n18 11 10 9 8 6 7\n80 75 73 71 71\n27 21 19 17 15 12 10 6\n43 38 36 34 33 28\n71 64 63 64 62\n97 91 89 88 87 88 89\n68 63 60 63 61 61\n17 11 13 10 9 7 5 1\n64 57 55 57 55 53 47\n73 66 63 60 59 59 56 53\n27 22 21 20 19 16 16 19\n31 25 25 24 24\n34 28 28 25 24 20\n73 68 65 65 64 59\n67 62 59 55 54 51 48 46\n76 70 68 64 61 58 60\n83 76 75 71 71\n76 71 69 68 64 60\n42 35 32 31 27 24 21 14\n52 46 44 43 36 33\n69 62 56 53 52 49 51\n23 18 12 10 9 7 7\n74 67 64 62 56 52\n28 21 15 12 9 2\n48 49 50 51 49\n48 51 52 53 54 56 56\n61 64 67 70 74\n14 17 20 23 29\n11 13 10 13 16\n26 28 31 30 33 30\n17 20 22 19 19\n59 61 63 65 62 66\n64 66 67 70 67 70 71 76\n20 22 22 25 28 31\n82 84 85 85 87 90 87\n32 33 36 37 37 39 41 41\n30 33 36 38 40 42 42 46\n83 85 86 89 92 92 93 99\n13 14 15 19 22 23 25\n20 21 25 28 26\n17 19 22 26 26\n35 36 40 42 45 49\n19 22 23 24 28 34\n37 39 40 46 49\n74 76 78 84 81\n23 25 28 35 35\n82 83 89 90 92 96\n39 41 47 48 53\n26 24 26 29 30 31 33 34\n8 7 10 11 9\n38 37 39 40 41 41\n39 38 40 42 46\n68 66 69 70 72 78\n60 57 59 60 58 61 62\n62 60 61 59 58\n11 10 12 14 13 13\n63 62 61 64 67 71\n89 86 85 88 90 91 97\n34 32 32 33 35\n16 14 14 17 18 19 18\n20 17 20 22 25 28 28 28\n6 5 6 6 10\n20 17 18 18 20 25\n20 19 22 26 29 32 35\n59 56 59 62 65 69 66\n72 70 71 75 76 79 82 82\n30 29 32 36 40\n80 79 83 86 93\n25 24 31 32 34 35 36\n12 9 14 16 13\n26 23 29 32 35 36 36\n23 22 27 28 32\n71 68 74 77 80 83 89\n81 81 84 87 90 93 95 97\n66 66 67 69 72 73 75 73\n75 75 76 78 80 82 82\n15 15 16 18 21 25\n51 51 53 56 61\n74 74 71 72 75\n34 34 36 35 32\n41 41 44 43 43\n72 72 69 71 75\n51 51 52 53 51 57\n53 53 55 58 59 59 61\n1 1 4 4 7 4\n76 76 79 79 82 83 83\n2 2 4 4 7 8 12\n85 85 86 88 88 93\n57 57 59 63 66 67\n65 65 68 69 71 72 76 75\n71 71 75 78 80 80\n29 29 33 36 40\n63 63 65 69 70 73 74 81\n11 11 17 18 21 24\n79 79 82 84 90 91 93 91\n15 15 20 23 25 25\n76 76 78 80 86 88 91 95\n38 38 39 46 51\n47 51 53 54 55\n33 37 38 41 40\n6 10 13 15 15\n50 54 56 59 60 62 66\n1 5 6 9 14\n44 48 46 48 50 52\n34 38 40 41 42 43 40 37\n77 81 82 83 82 83 83\n17 21 22 23 21 22 26\n74 78 80 78 80 83 88\n47 51 51 52 54 56\n65 69 72 74 75 75 77 76\n56 60 63 63 64 66 68 68\n53 57 58 59 59 61 65\n30 34 35 37 39 39 42 47\n72 76 78 82 85 88 89\n56 60 64 65 68 66\n83 87 90 94 94\n12 16 20 22 25 29\n45 49 53 56 63\n67 71 78 79 80 81 82\n75 79 82 84 89 92 91\n49 53 55 58 61 62 69 69\n58 62 64 69 70 73 76 80\n48 52 57 60 61 62 63 69\n45 52 54 56 58 60\n13 19 21 24 23\n25 31 32 33 34 37 37\n76 82 83 85 86 88 92\n48 55 56 58 64\n16 22 23 21 24 25 28\n25 32 34 36 35 32\n11 18 19 16 17 19 19\n25 31 32 34 37 40 38 42\n15 20 22 20 26\n67 72 75 75 76 79\n26 31 32 34 35 35 32\n71 77 80 81 83 85 85 85\n44 49 52 52 56\n66 71 71 72 73 76 78 83\n54 60 64 66 68 71 73\n36 42 45 46 50 52 49\n79 84 87 90 92 96 96\n30 37 40 42 44 45 49 53\n36 41 45 47 53\n45 52 59 61 63\n11 18 20 27 28 26\n16 21 28 31 31\n13 19 20 26 28 29 30 34\n56 61 62 65 72 75 82\n20 18 16 15 16\n37 34 33 32 31 30 30\n50 49 46 45 42 38\n61 58 55 53 51 48 43\n72 69 68 71 70 67 65\n13 10 11 9 8 7 10\n64 63 61 62 62\n48 46 47 46 42\n44 42 43 41 40 33\n11 8 8 5 2\n94 91 91 88 89\n47 46 43 43 40 40\n93 90 87 87 83\n25 23 20 18 18 12\n83 82 79 75 72\n38 35 32 29 25 23 24\n77 76 74 70 68 66 66\n85 83 81 78 77 75 71 67\n49 48 47 43 42 36\n79 78 75 73 72 70 63 61\n32 29 27 24 18 19\n48 47 40 38 38\n56 53 50 49 42 39 35\n25 22 21 20 15 8\n59 61 58 57 55 54\n64 67 64 61 60 58 61\n21 23 22 20 19 16 16\n68 69 67 65 64 60\n21 23 21 20 17 15 12 5\n11 12 10 7 8 6 4 3\n94 96 93 95 98\n77 79 77 78 75 72 72\n23 24 21 19 20 19 15\n32 34 33 34 27\n15 17 17 16 15\n34 37 35 32 31 31 33\n24 25 22 20 19 19 19\n15 18 15 15 13 11 7\n88 90 90 89 86 85 83 76\n81 84 81 77 74 72 69\n50 53 50 46 49\n51 53 49 48 48\n52 53 49 46 44 41 38 34\n57 60 58 55 51 48 41\n87 89 88 82 79\n64 65 64 61 54 55\n95 96 93 90 83 81 78 78\n17 20 18 11 9 6 5 1\n42 45 38 36 29\n48 48 47 44 41 40\n21 21 18 16 13 12 10 13\n64 64 63 60 57 57\n19 19 17 14 11 8 6 2\n18 18 15 13 8\n97 97 99 98 95 94\n42 42 40 42 40 38 39\n5 5 8 6 4 4\n66 66 64 63 61 64 62 58\n18 18 16 18 16 15 13 6\n23 23 23 20 17 16\n41 41 41 38 40\n36 36 34 31 28 28 28\n70 70 70 68 65 64 62 58\n21 21 19 19 14\n66 66 62 61 59 56 54 53\n78 78 74 71 68 67 66 68\n81 81 80 78 74 73 71 71\n69 69 68 64 60\n68 68 66 62 60 57 55 48\n55 55 48 45 43 42 40 38\n23 23 21 14 15\n61 61 60 57 56 51 51\n66 66 64 62 60 54 50\n96 96 90 87 81\n39 35 33 31 30 28 25\n35 31 28 26 23 26\n83 79 78 77 75 72 72\n42 38 37 35 31\n50 46 44 43 40 38 37 31\n74 70 69 71 69 68 65 63\n97 93 90 93 96\n43 39 38 35 38 35 35\n60 56 59 57 53\n87 83 82 85 83 77\n28 24 22 22 19 17\n52 48 48 45 48\n16 12 9 9 8 6 6\n94 90 88 86 86 84 81 77\n87 83 83 80 78 77 71\n66 62 61 57 56\n36 32 28 25 24 21 24\n64 60 59 56 52 52\n73 69 68 65 63 62 58 54\n76 72 68 67 65 62 56\n33 29 24 21 18\n94 90 88 81 83\n30 26 21 20 20\n42 38 36 30 26\n47 43 38 35 28\n40 35 32 31 28 25\n87 82 80 77 76 75 76\n16 10 8 5 2 2\n29 24 23 20 18 15 12 8\n57 50 48 45 40\n58 52 54 53 50 49\n86 80 77 78 76 73 75\n75 70 67 69 69\n66 60 57 58 55 54 51 47\n38 32 30 29 31 24\n54 47 45 43 43 41\n74 69 67 67 70\n52 47 44 44 41 39 37 37\n36 31 29 26 26 24 20\n32 26 25 23 22 22 21 16\n34 28 25 21 19\n86 80 76 73 75\n65 58 54 51 50 49 49\n50 43 41 38 37 35 31 27\n35 28 24 21 18 11\n34 27 20 18 16\n14 9 8 3 6\n63 57 56 49 49\n94 88 85 79 77 73\n70 65 60 57 56 55 52 45\n69 69 66 65 64 67 66 66\n62 64 66 66 63\n82 80 78 76 72 74\n90 92 92 93 94\n69 67 64 62 60\n42 45 47 48 51\n36 39 42 43 46 49 52\n13 10 9 7 5\n25 26 28 29 31 33 35 38\n14 13 12 9 7 4 2\n65 68 70 72 75 77\n23 26 28 29 31 34 35 37\n17 14 12 9 7 6 4 2\n33 34 37 38 41\n37 36 33 30 27\n5 8 9 10 12\n63 61 58 57 54\n23 25 28 31 33 35 38 39\n13 10 7 6 4\n86 88 90 91 92 94\n55 54 53 51 50 48 46\n33 30 27 24 21 18\n65 67 70 73 74 75 78\n53 51 48 45 43 40 37\n94 92 91 89 87 86 83\n69 71 74 75 76\n72 74 77 79 81 82 84 85\n21 18 17 14 11\n18 17 14 12 9\n88 91 92 94 96 99\n12 9 6 5 2\n68 65 62 61 60 57 55 53\n30 27 25 22 19 18 17\n34 37 39 42 44 47 49 51\n71 74 77 80 81 84 87\n29 28 25 23 20 17 14 12\n53 54 57 58 59\n23 21 19 17 16 15\n31 34 36 37 39 41 42 44\n58 61 64 66 68 70 72\n3 4 6 9 10 13 15\n64 63 60 58 56 54 53 52\n75 77 78 80 81 83 86 88\n61 63 65 66 68 71 73\n20 23 24 26 28 30 33\n18 15 12 10 7\n50 48 45 43 40 39 37 36\n48 47 45 42 41 39\n7 10 11 13 14 17 18\n25 22 19 18 16 14 12 11\n53 50 47 45 42\n57 58 59 60 62 64 65\n38 41 42 43 44 46 47\n50 53 56 57 60 62 64\n86 83 81 78 76 74 72 69\n60 61 64 66 68 69 71 73\n13 16 18 19 22 25 26 27\n77 79 81 84 85\n81 79 78 76 73 71\n32 34 37 40 42\n89 88 87 85 84 83\n59 62 65 67 68 70 73 74\n54 57 58 60 63\n34 32 31 29 27 24 21\n43 45 46 49 51\n76 78 79 80 82 84\n36 34 32 30 27\n23 24 25 28 29\n19 22 25 28 30 33 35\n61 59 57 54 52 50\n23 24 25 27 30 32 33 34\n63 64 66 69 70 71 72 74\n85 86 88 89 92 95 96 97\n8 9 10 13 15\n74 73 71 68 65 62 60 59\n51 48 45 44 41 38 36\n41 39 38 36 34 31\n58 59 60 62 64\n29 31 33 36 38 39 41 43\n1 2 4 5 7 10 11 14\n24 27 29 32 33 35\n54 52 50 49 47 45\n30 29 27 24 21 19 16 14\n44 46 49 50 51 52 54\n24 25 28 31 34 36\n53 52 51 50 47\n63 62 60 58 57 56 54 51\n38 39 41 44 47 49 52\n97 96 93 90 87 84 83\n81 79 77 75 72 70\n36 35 32 31 29\n76 77 80 83 86 89 91\n21 22 24 27 28 30 32 33\n80 81 82 84 85 86\n94 93 91 88 85 84 81 79\n77 79 81 82 85 86 89 91\n42 39 36 35 32 29 27\n19 18 16 15 14\n63 62 60 57 55 54 53 51\n31 30 27 25 23 22 20 18\n35 37 38 40 42 45 46\n41 44 45 47 48 51\n19 21 23 26 29 31\n32 30 28 26 23\n31 33 35 37 38\n46 43 40 37 35 32\n81 80 77 74 71\n16 17 18 19 22\n67 65 64 62 61\n66 63 61 58 55 54 51\n46 49 52 54 56\n51 50 47 46 43\n30 32 33 35 36\n66 63 60 58 57 56 55\n67 64 61 59 56 55\n37 36 33 32 31\n87 88 91 92 93 95 96\n62 59 58 55 54\n12 10 9 8 5\n64 62 61 60 58\n16 18 21 24 27 28 29\n79 78 76 73 71 70 69 66\n87 85 84 82 81 79 77\n74 76 79 81 82 83 86 89\n91 89 87 85 84 81 78 77\n75 72 71 68 65 64 62 59\n52 55 58 60 62 64\n44 46 49 51 53 54 55 56\n65 63 62 59 56 55 52 51\n21 23 25 28 31 32 34 37\n81 79 78 76 73 72 69 67\n14 11 10 9 8 6 3\n59 56 55 53 51 49\n62 59 58 56 54 53 52 49\n45 48 49 51 53 55 56\n11 13 16 18 20 21 24\n27 26 23 22 19 18\n70 67 64 62 60 57 55\n96 94 91 90 88 86\n63 66 67 70 73\n5 8 11 14 16 19 21 22\n83 85 86 88 91 92\n40 39 36 35 34\n73 75 77 78 81 83\n50 49 47 46 44 41 38 35\n73 76 79 81 83 86 88 90\n15 16 18 20 21\n85 87 88 89 91\n71 73 75 76 77 79 80 83\n28 25 22 21 20\n15 17 19 20 21 24 25\n40 39 37 34 31 30 27 25\n90 87 86 83 81 78 76 75\n32 31 28 25 24 21 18\n9 11 14 16 18\n9 10 11 14 16 19 21 22\n14 13 12 10 9 7 6\n53 50 48 46 43 42\n51 54 56 59 61 63 66 68\n39 41 43 44 47 50 53\n77 75 74 73 72 70 68\n40 43 46 48 50 52 54\n44 46 49 50 53 55 58 59\n13 14 15 18 20 21 24\n13 16 18 20 23 25\n16 19 20 23 26\n34 35 36 38 41 43\n62 59 58 56 55 53 52\n14 13 10 8 5 4 2\n14 13 12 9 6 4\n25 23 21 19 16 15\n64 63 60 57 55 53 50 48\n51 53 54 55 56 58 60\n74 72 69 66 64 63 61 58\n6 9 11 14 15 18 19\n69 72 73 76 78 79\n47 45 43 41 39\n30 28 26 23 20 19 17\n11 9 7 6 5 2 1\n53 51 50 49 48 45\n56 58 59 60 63 65 67\n88 85 84 81 79 78 77\n90 87 85 84 81 80 77\n49 46 45 42 40 37 36\n26 24 23 22 20 18\n45 43 42 41 38\n66 68 69 72 75\n25 24 21 20 17 14\n50 52 55 56 57 59 60 63\n15 14 13 10 9 6\n47 46 44 42 41 39\n75 77 79 80 83 85 86 88\n13 10 7 4 3\n95 92 89 87 84 81\n88 89 91 94 95 97 99\n79 76 74 71 68 66 65\n83 85 86 87 89 90 92\n44 45 46 48 51\n20 17 16 14 12\n59 60 63 65 67 70 72 75\n11 12 13 14 16 18\n63 60 59 57 55 53 51\n39 36 34 33 31 30 27 24\n61 59 58 56 53\n34 31 29 27 24\n66 67 68 70 73 74 77\n88 90 91 93 96 97 98\n58 59 61 64 65 68 71\n19 16 15 14 11 10\n34 31 28 25 23 21 18 16\n9 12 15 16 19 22\n11 13 16 19 20 23 24\n97 96 95 94 93 92 89\n33 30 29 28 26\n2 3 4 5 7 8\n57 60 61 64 67\n23 25 27 29 31 34 37 38\n70 67 65 64 61\n80 79 78 77 75 72\n43 40 38 37 34\n56 58 59 61 62 64\n54 57 58 60 62 64 67 69\n36 33 30 27 26 25\n48 50 53 56 59 62 63\n61 59 58 57 55 53\n81 82 83 85 86 89 92\n37 40 43 45 47 49 51\n99 97 95 92 89\n30 32 35 37 38\n52 53 54 57 59 60 61 62\n58 56 53 51 50\n30 29 26 25 24\n16 15 13 11 8 6\n26 23 21 20 19 18\n72 71 68 67 66 63 60\n18 15 13 12 9 6\n70 69 68 67 66 65\n64 66 68 71 72 74 76\n47 45 43 40 38\n9 12 13 14 15 17 19 20\n50 52 54 55 57 59 62 65\n17 15 14 12 10\n26 25 23 22 20 18 16 15\n82 80 78 77 75\n96 95 93 92 89\n22 23 25 28 29 32 33 35\n35 37 40 42 44 47 50\n74 73 70 69 67 64\n19 17 14 12 11 8 5 3\n62 64 67 68 70\n69 71 73 74 76 78\n7 8 9 10 13 14\n84 82 80 78 76 75 72\n18 21 24 25 26\n54 56 57 60 62 64 65\n62 63 66 68 70 73"

  private val reports = readInput(myInput)

  def day2task1: Int = {
    calculateSafeReports(reports, List.empty)
  }

  def day2task2: Int = {
    calculateSafeReports2(reports, List.empty)
  }

  private def readInput(input: String): List[List[Int]] = {
    val rows = input.split("\n").toList
    readRows(rows, List.empty)
  }

  @tailrec
  private def readRows(rows: List[String], collections: List[List[Int]]): List[List[Int]] = {
    rows.headOption match
      case Some(row) =>
        val numbersAsString = row.split(" ")
        readRows(rows.tail, collections :+ numbersAsString.map(e => e.toInt).toList)
      case None => collections
  }

  @tailrec
  private def calculateSafeReports(allReports: List[List[Int]], safeReports: List[List[Int]]): Int = {
    allReports.headOption match
      case Some(oneReport) =>
        if (isSafe(oneReport, None, None)) {
          calculateSafeReports(allReports.tail, safeReports :+ oneReport)
        } else {
          calculateSafeReports(allReports.tail, safeReports)
        }
      case None => safeReports.size
  }

  @tailrec
  private def calculateSafeReports2(allReports: List[List[Int]], safeReports: List[List[Int]]): Int = {
    allReports.headOption match
      case Some(oneReport) =>
        if (isKindaSafe(oneReport)) {
          calculateSafeReports2(allReports.tail, safeReports :+ oneReport)
        } else {
          calculateSafeReports2(allReports.tail, safeReports)
        }
      case None => safeReports.size
  }

  @tailrec
  private def isSafe(report: List[Int], ascending: Option[Boolean], previousLevel: Option[Int]): Boolean = {
    ascending match
      case Some(ascendingValue) =>
        report.headOption match
          case Some(currentLevel) =>
            if ((ascendingValue && previousLevel.get < currentLevel && currentLevel - previousLevel.get <= 3) ||
              (!ascendingValue && previousLevel.get > currentLevel && previousLevel.get - currentLevel <= 3)
            ) {
              isSafe(report.tail, Some(ascendingValue), Some(currentLevel))
            } else false
          case None => true
      case None =>
        val firstElement = report.head
        val secondElement = report.tail.head
        isSafe(report.tail, Some(secondElement > firstElement), Some(firstElement))
  }

  private def isKindaSafe(report: List[Int]): Boolean = {
   report.zipWithIndex.exists {
     case (e, index) =>
       val newList =
         if (index >= 0) report.patch(index, Nil, 1)
         else report
       isSafe(newList, None, None) || isSafe(report, None, None)
    }
  }
}