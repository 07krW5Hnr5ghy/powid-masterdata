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
public class WarehouseStockDTO {
    public String warehouse;
    public String supplierProductSerial;
    public Integer quantity;
    public Date registrationDate;
    public Date updateDate;
    public Long id;
    public Long productId;
}
