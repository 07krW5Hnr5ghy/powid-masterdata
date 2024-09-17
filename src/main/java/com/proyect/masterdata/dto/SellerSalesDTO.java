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
public class SellerSalesDTO {
    private String seller;
    private BigDecimal totalSales;
    private Long orderCount;
    private Integer productCount;
    private BigDecimal averageTicket;
}
