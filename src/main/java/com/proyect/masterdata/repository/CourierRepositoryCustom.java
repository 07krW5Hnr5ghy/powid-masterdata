package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Courier;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

@Repository
public interface CourierRepositoryCustom {
    Page<Courier> searchForCourier(
            String name,
            Long clientId,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status
    );
}
