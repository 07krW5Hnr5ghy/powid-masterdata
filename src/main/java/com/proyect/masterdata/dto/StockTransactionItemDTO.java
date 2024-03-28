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
public class StockTransactionItemDTO {
    private Integer quantity;
    private String warehouse;
    private String stockTransactionSerial;
    private String stockTransactionType;
    private String supplierProductSerial;
    private Date registrationDate;
}
