package com.proyect.masterdata.repository.impl;

import com.proyect.masterdata.domain.StockReplenishmentItem;
import com.proyect.masterdata.repository.StockReplenishmentItemRepositoryCustom;
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
import java.util.List;

@Repository
public class StockReplenishmentItemRepositoryCustomImpl implements StockReplenishmentItemRepositoryCustom {
    @PersistenceContext(name = "entityManager")
    private EntityManager entityManager;
    @Override
    public Page<StockReplenishmentItem> searchForStockReplenishmentItem(
            Long clientId,
            List<Long> orderIds,
            List<Long> productIds,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<StockReplenishmentItem> criteriaQuery = criteriaBuilder.createQuery(StockReplenishmentItem.class);
        Root<StockReplenishmentItem> itemRoot = criteriaQuery.from(StockReplenishmentItem.class);

        criteriaQuery.select(itemRoot);
        List<Predicate> conditions = predicate(
                clientId,
                orderIds,
                productIds,
                status,
                criteriaBuilder,
                itemRoot);

        if (!StringUtils.isBlank(sort) && !StringUtils.isBlank(sortColumn)) {

            List<Order> stockReplenishmentItemList = new ArrayList<>();

            if (sort.equalsIgnoreCase("ASC")) {
                stockReplenishmentItemList = listASC(sortColumn, criteriaBuilder, itemRoot);
            }

            if (sort.equalsIgnoreCase("DESC")) {
                stockReplenishmentItemList = listDESC(sortColumn, criteriaBuilder, itemRoot);
            }

            criteriaQuery.where(conditions.toArray(new Predicate[] {})).orderBy(stockReplenishmentItemList);
        } else {
            criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        }

        TypedQuery<StockReplenishmentItem> orderTypedQuery = entityManager.createQuery(criteriaQuery);
        orderTypedQuery.setFirstResult(pageNumber * pageSize);
        orderTypedQuery.setMaxResults(pageSize);

        Pageable pageable = PageRequest.of(pageNumber, pageSize);
        Long count = getOrderCount(
                clientId,
                orderIds,
                productIds,
                status);
        return new PageImpl<>(orderTypedQuery.getResultList(), pageable, count);
    }

    public List<Predicate> predicate(
            Long clientId,
            List<Long> orderIds,
            List<Long> productIds,
            Boolean status,
            CriteriaBuilder criteriaBuilder,
            Root<StockReplenishmentItem> itemRoot) {

        List<Predicate> conditions = new ArrayList<>();

        if (clientId != null) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("clientId"), clientId)));
        }

        if (!orderIds.isEmpty()) {
            conditions.add(criteriaBuilder.and(itemRoot.get("orderId").in(orderIds)));
        }

        if (!productIds.isEmpty()) {
            conditions.add(criteriaBuilder.and(itemRoot.get("productId").in(productIds)));
        }

        if (status) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.isTrue(itemRoot.get("status"))));
        }

        if (!status) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.isFalse(itemRoot.get("status"))));
        }

        return conditions;
    }

    private List<Order> listASC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<StockReplenishmentItem> itemRoot) {

        List<Order> stockReplenishmentList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("clientId")) {
            stockReplenishmentList.add(criteriaBuilder.asc(itemRoot.get("clientId")));
        }

        if (sortColumn.equalsIgnoreCase("orderId")) {
            stockReplenishmentList.add(criteriaBuilder.asc(itemRoot.get("orderId")));
        }

        if (sortColumn.equalsIgnoreCase("productId")) {
            stockReplenishmentList.add(criteriaBuilder.asc(itemRoot.get("productId")));
        }

        return stockReplenishmentList;
    }

    private List<Order> listDESC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<StockReplenishmentItem> itemRoot) {

        List<Order> stockReplenishmentList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("clientId")) {
            stockReplenishmentList.add(criteriaBuilder.desc(itemRoot.get("clientId")));
        }

        if (sortColumn.equalsIgnoreCase("orderId")) {
            stockReplenishmentList.add(criteriaBuilder.desc(itemRoot.get("orderId")));
        }

        if (sortColumn.equalsIgnoreCase("productId")) {
            stockReplenishmentList.add(criteriaBuilder.desc(itemRoot.get("productId")));
        }

        return stockReplenishmentList;
    }

    private Long getOrderCount(
            Long clientId,
            List<Long> orderIds,
            List<Long> productIds,
            Boolean status) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<StockReplenishmentItem> itemRoot = criteriaQuery.from(StockReplenishmentItem.class);

        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicate(
                clientId,
                orderIds,
                productIds,
                status,
                criteriaBuilder,
                itemRoot);
        criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }
}
