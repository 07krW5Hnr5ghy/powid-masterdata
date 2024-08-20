package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Audit;
import org.springframework.data.domain.Page;

import java.util.Date;

public interface AuditRepositoryCustom {
    Page<Audit> searchForAudit(
            Long userId,
            Long clientId,
            Long auditEventId,
            Date registrationStartDate,
            Date registrationEndDate,
            Date updateStartDate,
            Date updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize
    );
}
