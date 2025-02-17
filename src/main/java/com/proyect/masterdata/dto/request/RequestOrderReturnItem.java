package com.proyect.masterdata.dto.request;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.UUID;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class RequestOrderReturnItem {
    private UUID productId;
    private String supplierProductSerial;
    private Integer quantity;
    private String orderReturnType;
}
