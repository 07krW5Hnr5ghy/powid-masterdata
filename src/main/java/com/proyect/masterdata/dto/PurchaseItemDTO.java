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
public class PurchaseItemDTO {
    private String serial;
    private Integer quantity;
    private String supplierProductSerial;
    private Double unitPrice;
    private Date date;
}
