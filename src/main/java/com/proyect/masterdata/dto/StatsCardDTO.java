package com.proyect.masterdata.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class StatsCardDTO {
    BigDecimal totalSalesByStatus;
    Integer totalOrders;
    Integer totalOrdersByRangeDate;
    BigDecimal totalSalesByRangeDate;
    Integer totalProducts;
    BigDecimal averageSaleProduct;
    BigDecimal averageTicket;
    BigDecimal totalDeliveryAmountOrders;
    BigDecimal percentageOfOrders;
    BigDecimal percentageOfSales;
    String orderStatus;
}
