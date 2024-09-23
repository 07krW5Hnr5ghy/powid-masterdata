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
public class SalesByStatusDTO {
    private String status;
    private String customer;
    private String telephone;
    private String brand;
    private BigDecimal totalSales;
    private Integer totalOrders;
    private Integer totalProducts;
    private BigDecimal averageTicket;
}
