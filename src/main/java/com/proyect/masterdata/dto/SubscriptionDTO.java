package com.proyect.masterdata.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.OffsetDateTime;
import java.util.UUID;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class SubscriptionDTO {
    private String name;
    private Integer months;
    private Double discountPercent;
    private String user;
    private UUID id;
    private Boolean status;
    private OffsetDateTime registrationDate;
    private OffsetDateTime updateDate;
}
