package com.proyect.masterdata.dto.request;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class RequestStockTransaction {
    private String serial;
    private Integer quantity;
    private String stockTransactionType;
    private String warehouse;
    private String supplierProductSerial;
}
