package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Audit;
import org.springframework.data.domain.Page;

import java.time.OffsetDateTime;
import java.util.Date;
import java.util.UUID;

public interface AuditRepositoryCustom {
    Page<Audit> searchForAudit(
            UUID userId,
            UUID clientId,
            UUID auditEventId,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize
    );
}
