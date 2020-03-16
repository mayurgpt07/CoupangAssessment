--Calculate Spend Per Buyer using CTE
WITH cte_sales (user_id, test_option, SpendingPerBuyer)
AS
(
SELECT user_id, test_option, sum(abs(sale_price))
FROM CoupangSalesActivity WITH (NOLOCK)
WHERE abs(sale_price) IS NOT NULL
GROUP BY user_id, test_option
)

-- Caluculate the average
SELECT test_option, AVG(SpendingPerBuyer) AS AverageSpendPerBuyer
FROM cte_sales
GROUP BY test_option
