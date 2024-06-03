package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Audit;
import org.springframework.data.domain.Page;

public interface AuditRepositoryCustom {
    Page<Audit> searchForAudit(
            Long userId,
            Long clientId,
            Long auditEventId,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize
    );
}
