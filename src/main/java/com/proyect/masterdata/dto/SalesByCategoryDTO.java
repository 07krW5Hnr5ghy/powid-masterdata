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
public class SalesByCategoryDTO {
    private String closingChannel;
    private String category;
    private String brand;
    private BigDecimal totalSales;
    private Integer totalProducts;
    private Integer totalOrders;
    private BigDecimal averageTicket;
}
