package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Courier;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import java.util.Date;
import java.util.List;

@Repository
public interface CourierRepositoryCustom {
    Page<Courier> searchForCourier(
            Long clientId,
            List<String> names,
            Date registrationStartDate,
            Date registrationEndDate,
            Date updateStartDate,
            Date updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status
    );
}
