package com.proyect.masterdata.repository.impl;

import com.proyect.masterdata.domain.OrderReturnItem;
import com.proyect.masterdata.repository.OrderReturnItemRepositoryCustom;
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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

@Repository
public class OrderReturnItemCustomImpl implements OrderReturnItemRepositoryCustom {
    @PersistenceContext(name = "entityManager")
    private EntityManager entityManager;
    @Override
    public Page<OrderReturnItem> searchForOrderReturnItem(
            Long orderId,
            Long clientId,
            Long productId,
            Long supplierProductId,
            Long warehouseId,
            Long orderReturnTypeId,
            Date registrationStartDate,
            Date registrationEndDate,
            Date updateStartDate,
            Date updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<OrderReturnItem> criteriaQuery = criteriaBuilder.createQuery(OrderReturnItem.class);
        Root<OrderReturnItem> itemRoot = criteriaQuery.from(OrderReturnItem.class);

        criteriaQuery.select(itemRoot);
        List<Predicate> conditions = predicateConditions(
                orderId,
                clientId,
                productId,
                supplierProductId,
                warehouseId,
                orderReturnTypeId,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate,
                status,
                criteriaBuilder,
                itemRoot);

        if (!StringUtils.isBlank(sort) && !StringUtils.isBlank(sortColumn)) {
            List<Order> orderReturnItemList = new ArrayList<>();
            if (sort.equalsIgnoreCase("ASC")) {
                orderReturnItemList = listASC(sortColumn, criteriaBuilder, itemRoot);
            }
            if (sort.equalsIgnoreCase("DESC")) {
                orderReturnItemList = listDESC(sortColumn, criteriaBuilder, itemRoot);
            }
            criteriaQuery.where(conditions.toArray(new Predicate[] {})).orderBy(orderReturnItemList);
        } else {
            criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        }

        TypedQuery<OrderReturnItem> orderTypedQuery = entityManager.createQuery(criteriaQuery);
        orderTypedQuery.setFirstResult(pageNumber * pageSize);
        orderTypedQuery.setMaxResults(pageSize);

        Pageable pageable = PageRequest.of(pageNumber, pageSize);
        long count = getOrderCount(
                orderId,
                clientId,
                productId,
                supplierProductId,
                warehouseId,
                orderReturnTypeId,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate,
                status);
        return new PageImpl<>(orderTypedQuery.getResultList(), pageable, count);
    }
    List<Predicate> predicateConditions(
            Long orderId,
            Long clientId,
            Long productId,
            Long supplierProductId,
            Long warehouseId,
            Long orderReturnTypeId,
            Date registrationStartDate,
            Date registrationEndDate,
            Date updateStartDate,
            Date updateEndDate,
            Boolean status,
            CriteriaBuilder criteriaBuilder,
            Root<OrderReturnItem> itemRoot
    ) {
        List<Predicate> conditions = new ArrayList<>();

        if (orderId != null) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("orderId"), orderId)));
        }

        if (clientId != null) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("clientId"), clientId)));
        }

        if(productId != null){
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("productId"), productId)));
        }

        if(supplierProductId != null){
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("supplierProductId"), supplierProductId)));
        }

        if (warehouseId != null) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("warehouseId"), clientId)));
        }

        if (orderReturnTypeId != null) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("orderReturnTypeId"), orderReturnTypeId)));
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
            Root<OrderReturnItem> itemRoot) {
        List<Order> orderReturnItemList = new ArrayList<>();
        if (sortColumn.equalsIgnoreCase("orderId")) {
            orderReturnItemList.add(criteriaBuilder.asc(itemRoot.get("orderId")));
        }
        if (sortColumn.equalsIgnoreCase("clientId")) {
            orderReturnItemList.add(criteriaBuilder.asc(itemRoot.get("clientId")));
        }
        if (sortColumn.equalsIgnoreCase("productId")) {
            orderReturnItemList.add(criteriaBuilder.asc(itemRoot.get("productId")));
        }
        if (sortColumn.equalsIgnoreCase("supplierProductId")) {
            orderReturnItemList.add(criteriaBuilder.asc(itemRoot.get("supplierProductId")));
        }
        if (sortColumn.equalsIgnoreCase("warehouseId")) {
            orderReturnItemList.add(criteriaBuilder.asc(itemRoot.get("warehouseId")));
        }
        if (sortColumn.equalsIgnoreCase("orderReturnTypeId")) {
            orderReturnItemList.add(criteriaBuilder.asc(itemRoot.get("orderReturnTypeId")));
        }
        if (sortColumn.equalsIgnoreCase("registrationStartDate")) {
            orderReturnItemList.add(criteriaBuilder.asc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("registrationEndDate")) {
            orderReturnItemList.add(criteriaBuilder.asc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateStartDate")) {
            orderReturnItemList.add(criteriaBuilder.asc(itemRoot.get("updateDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateEndDate")) {
            orderReturnItemList.add(criteriaBuilder.asc(itemRoot.get("updateDate")));
        }
        return orderReturnItemList;
    }

    List<Order> listDESC(
            String sortColumn,
            CriteriaBuilder criteriaBuilder,
            Root<OrderReturnItem> itemRoot) {
        List<Order> orderReturnItemList = new ArrayList<>();
        if (sortColumn.equalsIgnoreCase("orderId")) {
            orderReturnItemList.add(criteriaBuilder.desc(itemRoot.get("orderId")));
        }
        if (sortColumn.equalsIgnoreCase("clientId")) {
            orderReturnItemList.add(criteriaBuilder.desc(itemRoot.get("clientId")));
        }
        if (sortColumn.equalsIgnoreCase("productId")) {
            orderReturnItemList.add(criteriaBuilder.desc(itemRoot.get("productId")));
        }
        if (sortColumn.equalsIgnoreCase("supplierProductId")) {
            orderReturnItemList.add(criteriaBuilder.desc(itemRoot.get("supplierProductId")));
        }
        if (sortColumn.equalsIgnoreCase("warehouseId")) {
            orderReturnItemList.add(criteriaBuilder.desc(itemRoot.get("warehouseId")));
        }
        if (sortColumn.equalsIgnoreCase("orderReturnTypeId")) {
            orderReturnItemList.add(criteriaBuilder.desc(itemRoot.get("orderReturnTypeId")));
        }
        if (sortColumn.equalsIgnoreCase("registrationStartDate")) {
            orderReturnItemList.add(criteriaBuilder.desc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("registrationEndDate")) {
            orderReturnItemList.add(criteriaBuilder.desc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateStartDate")) {
            orderReturnItemList.add(criteriaBuilder.desc(itemRoot.get("updateDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateEndDate")) {
            orderReturnItemList.add(criteriaBuilder.desc(itemRoot.get("updateDate")));
        }
        return orderReturnItemList;
    }

    private long getOrderCount(
            Long orderId,
            Long clientId,
            Long productId,
            Long supplierProductId,
            Long warehouseId,
            Long orderReturnTypeId,
            Date registrationStartDate,
            Date registrationEndDate,
            Date updateStartDate,
            Date updateEndDate,
            Boolean status) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<OrderReturnItem> itemRoot = criteriaQuery.from(OrderReturnItem.class);

        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicateConditions(
                orderId,
                clientId,
                productId,
                supplierProductId,
                warehouseId,
                orderReturnTypeId,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate,
                status,
                criteriaBuilder,
                itemRoot);
        criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }
}
