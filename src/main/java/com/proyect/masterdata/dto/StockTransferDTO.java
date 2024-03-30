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
public class StockTransferDTO {
    private Long stockTransferId;
    private String serial;
    private String originWarehouse;
    private String destinationWarehouse;
    private Date registrationDate;
}
