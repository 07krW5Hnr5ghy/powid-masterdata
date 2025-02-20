package com.proyect.masterdata.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.OffsetDateTime;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class PurchasePaymentTypeDTO {
    private String name;
    private OffsetDateTime registrationDate;
    private OffsetDateTime updateDate;
    private Boolean status;
}
