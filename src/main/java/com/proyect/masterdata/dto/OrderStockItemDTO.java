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
public class OrderStockItemDTO {
    private Long orderId;
    private Long orderItemId;
    private String warehouse;
    private String supplierProductSerial;
    private String productSku;
    private Integer quantity;
    private Date registrationDate;
    private Date updateDate;
}
