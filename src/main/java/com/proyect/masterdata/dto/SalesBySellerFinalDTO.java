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
public class SalesBySellerFinalDTO {
    private String seller;
    private String department;
    private String province;
    private String district;
    private String closingChannel;
    private String category;
    private String brand;
    private Integer totalProducts;
    private Integer totalOrders;
    private BigDecimal averageTicket;
    private BigDecimal totalSales;
    private Integer TotalDeliveredOrders;
}
