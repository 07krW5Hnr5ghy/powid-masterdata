package com.proyect.masterdata.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Date;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class StockTransferItemDTO {
    private Long stockTransferId;
    private Integer quantity;
    private String originWarehouse;
    private String destinationWarehouse;
    private String supplierProductSerial;
    private Date registrationDate;
}
