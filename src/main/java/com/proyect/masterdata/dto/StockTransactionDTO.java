package com.proyect.masterdata.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class StockTransactionDTO {
    private Integer quantity;
    private String warehouse;
    private String stockTransactionType;
    private String supplierProductSerial;
}
