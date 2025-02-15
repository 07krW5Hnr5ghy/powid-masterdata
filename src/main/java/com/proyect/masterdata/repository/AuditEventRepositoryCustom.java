package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.AuditEvent;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import java.time.OffsetDateTime;
import java.util.Date;
@Repository
public interface AuditEventRepositoryCustom {
    Page<AuditEvent> searchForAuditEvent(
            String name,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status
    );
}
