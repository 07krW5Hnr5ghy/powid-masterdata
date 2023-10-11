package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Client;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

@Repository
public interface ClientRepositoryCustom {
    Page<Client> searchForClient(
            String ruc,
            String business,
            String user,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Long status
    );
}
