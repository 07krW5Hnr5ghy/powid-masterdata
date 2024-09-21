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
public class SalesCategoryDTO {
    private String category;
    private String saleChannel;
    private BigDecimal totalSales;
    private Integer totalProducts;
    private Integer totalOrders;
    private BigDecimal averageTicket;
}
