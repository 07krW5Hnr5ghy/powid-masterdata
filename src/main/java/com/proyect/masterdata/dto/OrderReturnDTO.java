package com.proyect.masterdata.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.OffsetDateTime;
import java.util.Date;
import java.util.UUID;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class OrderReturnDTO {
    private UUID orderId;
    private String warehouse;
    private OffsetDateTime registrationDate;
    private OffsetDateTime updateDate;
}
