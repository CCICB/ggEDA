# ggEDA heirarchical sort works

    Code
      ggstack(data = mock_data, col_sort = c("Category2", "Logical"), return = "data",
      verbose = FALSE)
    Output
         Category2 Logical ID Category Numeric Tooltip DefaultID
      1          A    TRUE  1        A       1       a         1
      3          A    TRUE  3        C       3       c         3
      5          A    TRUE  5        E       5       e         5
      6          A    TRUE  6        A       6       f         6
      7          A    TRUE  7        B       7       g         7
      2          A   FALSE  2        B       2       b         2
      4          A   FALSE  4        D       4       d         4
      8          B    TRUE  8        C       8       h         8
      9          B    TRUE  9        D       9       i         9
      10         B   FALSE 10        E      10       j        10

---

    Code
      sorted_result
    Output
         Category2 Logical ID Category Numeric Tooltip DefaultID
      1          A    TRUE  1        A       1       a         1
      3          A    TRUE  3        C       3       c         3
      5          A    TRUE  5        E       5       e         5
      6          A    TRUE  6        A       6       f         6
      7          A    TRUE  7        B       7       g         7
      2          A   FALSE  2        B       2       b         2
      4          A   FALSE  4        D       4       d         4
      8          B    TRUE  8        C       8       h         8
      9          B    TRUE  9        D       9       i         9
      10         B   FALSE 10        E      10       j        10

---

    Code
      ggstack(data = mock_data, col_sort = c("Category2", "Logical"), sort_type = "alphabetical",
      desc = FALSE, order_matches_sort = TRUE, return = "data", verbose = FALSE)
    Output
         Category2 Logical ID Category Numeric Tooltip DefaultID
      2          A   FALSE  2        B       2       b         2
      4          A   FALSE  4        D       4       d         4
      1          A    TRUE  1        A       1       a         1
      3          A    TRUE  3        C       3       c         3
      5          A    TRUE  5        E       5       e         5
      6          A    TRUE  6        A       6       f         6
      7          A    TRUE  7        B       7       g         7
      10         B   FALSE 10        E      10       j        10
      8          B    TRUE  8        C       8       h         8
      9          B    TRUE  9        D       9       i         9

---

    Code
      ggstack(data = mock_data, col_sort = c("Category2", "Logical"), sort_type = "alphabetical",
      desc = TRUE, order_matches_sort = TRUE, return = "data", verbose = FALSE)
    Output
         Category2 Logical ID Category Numeric Tooltip DefaultID
      8          B    TRUE  8        C       8       h         8
      9          B    TRUE  9        D       9       i         9
      10         B   FALSE 10        E      10       j        10
      1          A    TRUE  1        A       1       a         1
      3          A    TRUE  3        C       3       c         3
      5          A    TRUE  5        E       5       e         5
      6          A    TRUE  6        A       6       f         6
      7          A    TRUE  7        B       7       g         7
      2          A   FALSE  2        B       2       b         2
      4          A   FALSE  4        D       4       d         4

