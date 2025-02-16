package com.proyect.masterdata.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.OffsetDateTime;
import java.util.Date;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class StockTransferItemDTO {
    private String serial;
    private Integer quantity;
    private String origin;
    private String destination;
    private String supplierProduct;
    private OffsetDateTime registrationDate;
}
