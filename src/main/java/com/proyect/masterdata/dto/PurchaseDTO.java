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
public class PurchaseDTO {
    private String serial;
    private String purchaseDocument;
    private String warehouse;
    private String purchaseType;
    private OffsetDateTime registrationDate;
}
