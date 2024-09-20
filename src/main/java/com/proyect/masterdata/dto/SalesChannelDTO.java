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
public class SalesChannelDTO {
    private String saleChannel;
    private BigDecimal totalSales;
    private Long totalOrders;
    private Integer totalProducts;
    private BigDecimal averageTicket;
}
