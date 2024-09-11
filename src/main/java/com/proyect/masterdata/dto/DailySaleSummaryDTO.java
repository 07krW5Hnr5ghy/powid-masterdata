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
public class DailySaleSummaryDTO {
    BigDecimal totalSalePerDay;
    Integer totalOrders;
    Date date;
}
