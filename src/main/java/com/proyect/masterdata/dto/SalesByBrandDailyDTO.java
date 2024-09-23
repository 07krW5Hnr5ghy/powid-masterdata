package com.proyect.masterdata.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.util.Date;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class SalesByBrandDailyDTO {
    private Date date;
    private String seller;
    private String brand;
    private BigDecimal totalSales;
    private Integer totalOrders;
    private BigDecimal averageTicket;
    private Integer totalProducts;
}
