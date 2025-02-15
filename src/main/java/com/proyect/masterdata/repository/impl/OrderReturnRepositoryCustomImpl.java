package com.proyect.masterdata.repository.impl;

import com.proyect.masterdata.domain.OrderReturn;
import com.proyect.masterdata.domain.OrderStock;
import com.proyect.masterdata.repository.OrderReturnRepositoryCustom;
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
import java.util.Date;
import java.util.List;
import java.util.UUID;

@Repository
public class OrderReturnRepositoryCustomImpl implements OrderReturnRepositoryCustom {
    @PersistenceContext(name = "entityManager")
    private EntityManager entityManager;
    @Override
    public Page<OrderReturn> searchForOrderReturn(
            UUID clientId,
            List<UUID> orderIds,
            List<UUID> warehouseIds,
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
        CriteriaQuery<OrderReturn> criteriaQuery = criteriaBuilder.createQuery(OrderReturn.class);
        Root<OrderReturn> itemRoot = criteriaQuery.from(OrderReturn.class);
        Join<OrderReturn, OrderStock> orderReturnOrderStockJoin = itemRoot.join("orderStock");
        
        criteriaQuery.select(itemRoot);
        List<Predicate> conditions = predicateConditions(
                clientId,
                orderIds,
                warehouseIds,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate,
                status,
                criteriaBuilder,
                itemRoot,
                orderReturnOrderStockJoin);

        if (!StringUtils.isBlank(sort) && !StringUtils.isBlank(sortColumn)) {
            List<Order> orderReturnList = new ArrayList<>();
            if (sort.equalsIgnoreCase("ASC")) {
                orderReturnList = listASC(sortColumn, criteriaBuilder, itemRoot);
            }
            if (sort.equalsIgnoreCase("DESC")) {
                orderReturnList = listDESC(sortColumn, criteriaBuilder, itemRoot);
            }
            criteriaQuery.where(conditions.toArray(new Predicate[] {})).orderBy(orderReturnList);
        } else {
            criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        }

        TypedQuery<OrderReturn> orderTypedQuery = entityManager.createQuery(criteriaQuery);
        orderTypedQuery.setFirstResult(pageNumber * pageSize);
        orderTypedQuery.setMaxResults(pageSize);

        Pageable pageable = PageRequest.of(pageNumber, pageSize);
        long count = getOrderCount(
                clientId,
                orderIds,
                warehouseIds,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate,
                status);
        return new PageImpl<>(orderTypedQuery.getResultList(), pageable, count);
    }
    
    List<Predicate> predicateConditions(
            UUID clientId,
            List<UUID> orderIds,
            List<UUID> warehouseIds,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            Boolean status,
            CriteriaBuilder criteriaBuilder,
            Root<OrderReturn> itemRoot,
            Join<OrderReturn, OrderStock> orderReturnOrderStockJoin
    ) {
        List<Predicate> conditions = new ArrayList<>();

        if (!orderIds.isEmpty()) {
            conditions.add(criteriaBuilder.and(itemRoot.get("orderId").in(orderIds)));
        }

        if (clientId != null) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("clientId"), clientId)));
        }

        if (!warehouseIds.isEmpty()) {
            conditions.add(criteriaBuilder.and(orderReturnOrderStockJoin.get("warehouseId").in(warehouseIds)));
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

    List<Order> listASC(
            String sortColumn,
            CriteriaBuilder criteriaBuilder,
            Root<OrderReturn> itemRoot) {
        List<Order> orderReturnList = new ArrayList<>();
        if (sortColumn.equalsIgnoreCase("orderId")) {
            orderReturnList.add(criteriaBuilder.asc(itemRoot.get("orderId")));
        }
        if (sortColumn.equalsIgnoreCase("clientId")) {
            orderReturnList.add(criteriaBuilder.asc(itemRoot.get("clientId")));
        }
        if (sortColumn.equalsIgnoreCase("warehouseId")) {
            orderReturnList.add(criteriaBuilder.asc(itemRoot.get("warehouseId")));
        }
        if (sortColumn.equalsIgnoreCase("registrationStartDate")) {
            orderReturnList.add(criteriaBuilder.asc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("registrationEndDate")) {
            orderReturnList.add(criteriaBuilder.asc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateStartDate")) {
            orderReturnList.add(criteriaBuilder.asc(itemRoot.get("updateDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateEndDate")) {
            orderReturnList.add(criteriaBuilder.asc(itemRoot.get("updateDate")));
        }
        return orderReturnList;
    }

    List<Order> listDESC(
            String sortColumn,
            CriteriaBuilder criteriaBuilder,
            Root<OrderReturn> itemRoot) {
        List<Order> orderReturnList = new ArrayList<>();
        if (sortColumn.equalsIgnoreCase("orderId")) {
            orderReturnList.add(criteriaBuilder.desc(itemRoot.get("orderId")));
        }
        if (sortColumn.equalsIgnoreCase("clientId")) {
            orderReturnList.add(criteriaBuilder.desc(itemRoot.get("clientId")));
        }
        if (sortColumn.equalsIgnoreCase("warehouseId")) {
            orderReturnList.add(criteriaBuilder.desc(itemRoot.get("warehouseId")));
        }
        if (sortColumn.equalsIgnoreCase("registrationStartDate")) {
            orderReturnList.add(criteriaBuilder.desc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("registrationEndDate")) {
            orderReturnList.add(criteriaBuilder.desc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateStartDate")) {
            orderReturnList.add(criteriaBuilder.desc(itemRoot.get("updateDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateEndDate")) {
            orderReturnList.add(criteriaBuilder.desc(itemRoot.get("updateDate")));
        }
        return orderReturnList;
    }

    private long getOrderCount(
            UUID clientId,
            List<UUID> orderIds,
            List<UUID> warehouseIds,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            Boolean status) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<OrderReturn> itemRoot = criteriaQuery.from(OrderReturn.class);
        Join<OrderReturn, OrderStock> orderReturnOrderStockJoin = itemRoot.join("orderStock");

        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicateConditions(
                clientId,
                orderIds,
                warehouseIds,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate,
                status,
                criteriaBuilder,
                itemRoot,
                orderReturnOrderStockJoin);
        criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }
}
