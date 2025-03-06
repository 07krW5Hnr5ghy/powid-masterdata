package com.proyect.masterdata.repository.impl;

import com.proyect.masterdata.domain.Courier;
import com.proyect.masterdata.domain.Warehouse;
import com.proyect.masterdata.domain.WarehouseOutput;
import com.proyect.masterdata.repository.WarehouseOutputRepositoryCustom;
import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.TypedQuery;
import jakarta.persistence.criteria.*;
import org.apache.commons.lang3.StringUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;

import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@Repository
public class WarehouseOutputRepositoryCustomImpl implements WarehouseOutputRepositoryCustom {
    @PersistenceContext(name = "entityManager")
    private EntityManager entityManager;
    @Override
    public Page<WarehouseOutput> searchForWarehouseOutput(
            UUID clientId,
            Long orderNumber,
            String ref,
            String courier,
            String warehouse,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<WarehouseOutput> criteriaQuery = criteriaBuilder.createQuery(WarehouseOutput.class);
        Root<WarehouseOutput> itemRoot = criteriaQuery.from(WarehouseOutput.class);
        Join<WarehouseOutput, Courier> warehouseOutputCourierJoin = itemRoot.join("courier");
        Join<WarehouseOutput, Warehouse> warehouseOutputWarehouseJoin = itemRoot.join("warehouse");
        criteriaQuery.select(itemRoot);
        List<Predicate> conditions = predicate(
                clientId,
                orderNumber,
                ref,
                courier,
                warehouse,
                status,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate,
                criteriaBuilder,
                itemRoot,
                warehouseOutputCourierJoin,
                warehouseOutputWarehouseJoin
        );
        if (!StringUtils.isBlank(sort) && !StringUtils.isBlank(sortColumn)) {

            List<Order> warehouseOutputList = new ArrayList<>();

            if (sort.equalsIgnoreCase("ASC")) {
                warehouseOutputList = listASC(sortColumn, criteriaBuilder, itemRoot);
            }

            if (sort.equalsIgnoreCase("DESC")) {
                warehouseOutputList = listDESC(sortColumn, criteriaBuilder, itemRoot);
            }

            criteriaQuery.where(conditions.toArray(new Predicate[] {})).orderBy(warehouseOutputList);
        } else {
            criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        }
        TypedQuery<WarehouseOutput> orderTypedQuery = entityManager.createQuery(criteriaQuery);
        orderTypedQuery.setFirstResult(pageNumber * pageSize);
        orderTypedQuery.setMaxResults(pageSize);

        Pageable pageable = PageRequest.of(pageNumber, pageSize);
        Long count = getWarehouseOutputCount(
                clientId,
                orderNumber,
                ref,
                courier,
                warehouse,
                status,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate);
        return new PageImpl<>(orderTypedQuery.getResultList(), pageable, count);
    }

    private List<Predicate> predicate(
            UUID clientId,
            Long orderNumber,
            String ref,
            String courier,
            String warehouse,
            Boolean status,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            CriteriaBuilder criteriaBuilder,
            Root<WarehouseOutput> itemRoot,
            Join<WarehouseOutput,Courier> warehouseOutputCourierJoin,
            Join<WarehouseOutput,Warehouse> warehouseOutputWarehouseJoin
    ){
        List<Predicate> conditions = new ArrayList<>();
        if (clientId != null) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("clientId"), clientId)));
        }
        if(orderNumber!=null){
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("orderNumber"), orderNumber)));
        }
        if(ref != null){
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(itemRoot.get("ref")),"%"+ref.toUpperCase()+"%"));
        }

        if(courier != null){
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(warehouseOutputCourierJoin.get("name")),"%"+courier.toUpperCase()+"%"));
        }

        if(warehouse != null){
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(warehouseOutputWarehouseJoin.get("name")),"%"+warehouse.toUpperCase()+"%"));
        }

        if(registrationStartDate!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.greaterThanOrEqualTo(itemRoot.get("registrationDate"),registrationStartDate)
                    )
            );
        }

        if(registrationEndDate!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.lessThanOrEqualTo(itemRoot.get("registrationDate"),registrationEndDate)
                    )
            );
        }

        if(updateStartDate!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.greaterThanOrEqualTo(itemRoot.get("updateDate"),updateStartDate)
                    )
            );
        }

        if(updateEndDate!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.lessThanOrEqualTo(itemRoot.get("updateDate"),updateEndDate)
                    )
            );
        }

        if (status) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.isTrue(itemRoot.get("status"))));
        }

        if (!status) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.isFalse(itemRoot.get("status"))));
        }

        return conditions;
    }

    private List<Order> listASC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<WarehouseOutput> itemRoot) {

        List<Order> warehouseOutputList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("orderNumber")) {
            warehouseOutputList.add(criteriaBuilder.asc(itemRoot.get("orderNumber")));
        }

        if (sortColumn.equalsIgnoreCase("clientId")) {
            warehouseOutputList.add(criteriaBuilder.asc(itemRoot.get("clientId")));
        }

        if(sortColumn.equalsIgnoreCase("updateDate")){
            warehouseOutputList.add(criteriaBuilder.asc(itemRoot.get(
                    "updateDate"
            )));
        }

        if(sortColumn.equalsIgnoreCase("registrationDate")){
            warehouseOutputList.add(criteriaBuilder.asc(itemRoot.get(
                    "registrationDate"
            )));
        }

        return warehouseOutputList;

    }

    private List<Order> listDESC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<WarehouseOutput> itemRoot) {

        List<Order> warehouseOutputList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("orderNumber")) {
            warehouseOutputList.add(criteriaBuilder.asc(itemRoot.get("orderNumber")));
        }

        if (sortColumn.equalsIgnoreCase("name")) {
            warehouseOutputList.add(criteriaBuilder.desc(itemRoot.get("name")));
        }

        if (sortColumn.equalsIgnoreCase("clientId")) {
            warehouseOutputList.add(criteriaBuilder.desc(itemRoot.get("clientId")));
        }

        if(sortColumn.equalsIgnoreCase("updateDate")){
            warehouseOutputList.add(criteriaBuilder.asc(itemRoot.get(
                    "updateDate"
            )));
        }

        if(sortColumn.equalsIgnoreCase("registrationDate")){
            warehouseOutputList.add(criteriaBuilder.asc(itemRoot.get(
                    "registrationDate"
            )));
        }

        return warehouseOutputList;

    }

    private Long getWarehouseOutputCount(
            UUID clientId,
            Long orderNumber,
            String ref,
            String courier,
            String warehouse,
            Boolean status,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate
    ){
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<WarehouseOutput> itemRoot = criteriaQuery.from(WarehouseOutput.class);
        Join<WarehouseOutput, Courier> warehouseOutputCourierJoin = itemRoot.join("courier");
        Join<WarehouseOutput, Warehouse> warehouseOutputWarehouseJoin = itemRoot.join("warehouse");
        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicate(
                clientId,
                orderNumber,
                ref,
                courier,
                warehouse,
                status,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate,
                criteriaBuilder,
                itemRoot,
                warehouseOutputCourierJoin,
                warehouseOutputWarehouseJoin
        );
        criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }
}
