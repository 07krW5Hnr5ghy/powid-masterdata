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
public class SalesStatusDTO {
    private String status;
    private BigDecimal totalSales;
    private Long totalOrders;
    private Integer totalProducts;
    private BigDecimal averageTicket;
}
