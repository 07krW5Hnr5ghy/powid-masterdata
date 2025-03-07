package com.proyect.masterdata.dto;

import java.time.OffsetDateTime;
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
    private String serial;
    private String transactionType;
    private String supplierProduct;
    private OffsetDateTime registrationDate;
    private String user;
}
