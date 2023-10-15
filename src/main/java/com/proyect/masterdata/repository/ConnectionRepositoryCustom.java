package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Connection;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

@Repository
public interface ConnectionRepositoryCustom {
    Page<Connection> searchForConnection(
            String url,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status
    );
}
