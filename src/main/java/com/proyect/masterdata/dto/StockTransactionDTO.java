package com.proyect.masterdata.dto;

import java.util.Date;

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
    private Date date;
}
