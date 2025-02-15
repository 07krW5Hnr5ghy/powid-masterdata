package com.proyect.masterdata.repository.impl;

import com.proyect.masterdata.domain.StockReturn;
import com.proyect.masterdata.repository.StockReturnRepositoryCustom;
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
import java.util.UUID;

@Repository
public class StockReturnRepositoryCustomImpl implements StockReturnRepositoryCustom {
    @PersistenceContext(name = "entityManager")
    private EntityManager entityManager;
    @Override
    public Page<StockReturn> searchForStockReturn(
            UUID clientId,
            String serial,
            List<UUID> supplierIds,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<StockReturn> criteriaQuery = criteriaBuilder.createQuery(StockReturn.class);
        Root<StockReturn> itemRoot = criteriaQuery.from(StockReturn.class);

        criteriaQuery.select(itemRoot);
        List<Predicate> conditions = predicate(
                clientId,
                serial,
                supplierIds,
                status,
                criteriaBuilder,
                itemRoot);

        if (!StringUtils.isBlank(sort) && !StringUtils.isBlank(sortColumn)) {

            List<Order> stockReturnList = new ArrayList<>();

            if (sort.equalsIgnoreCase("ASC")) {
                stockReturnList = listASC(sortColumn, criteriaBuilder, itemRoot);
            }

            if (sort.equalsIgnoreCase("DESC")) {
                stockReturnList = listDESC(sortColumn, criteriaBuilder, itemRoot);
            }

            criteriaQuery.where(conditions.toArray(new Predicate[] {})).orderBy(stockReturnList);
        } else {
            criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        }

        TypedQuery<StockReturn> orderTypedQuery = entityManager.createQuery(criteriaQuery);
        orderTypedQuery.setFirstResult(pageNumber * pageSize);
        orderTypedQuery.setMaxResults(pageSize);

        Pageable pageable = PageRequest.of(pageNumber, pageSize);
        Long count = getOrderCount(
                clientId,
                serial,
                supplierIds,
                status);
        return new PageImpl<>(orderTypedQuery.getResultList(), pageable, count);
    }

    public List<Predicate> predicate(
            UUID clientId,
            String serial,
            List<UUID> supplierIds,
            Boolean status,
            CriteriaBuilder criteriaBuilder,
            Root<StockReturn> itemRoot) {

        List<Predicate> conditions = new ArrayList<>();

        if (clientId != null) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("clientId"), clientId)));
        }

        if (serial!=null) {
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(itemRoot.get("serial")),"%"+serial.toUpperCase()+"%"));
        }

        if(!supplierIds.isEmpty()){
            conditions.add(criteriaBuilder.and(itemRoot.get("supplierId").in(supplierIds)));
        }

        if (status) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.isTrue(itemRoot.get("status"))));
        }

        if (!status) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.isFalse(itemRoot.get("status"))));
        }

        return conditions;
    }

    private List<Order> listASC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<StockReturn> itemRoot) {

        List<Order> stockReturnList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("clientId")) {
            stockReturnList.add(criteriaBuilder.asc(itemRoot.get("clientId")));
        }

        return stockReturnList;
    }

    private List<Order> listDESC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<StockReturn> itemRoot) {

        List<Order> stockReturnList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("clientId")) {
            stockReturnList.add(criteriaBuilder.desc(itemRoot.get("clientId")));
        }

        return stockReturnList;
    }

    private Long getOrderCount(
            UUID clientId,
            String serial,
            List<UUID> supplierIds,
            Boolean status) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<StockReturn> itemRoot = criteriaQuery.from(StockReturn.class);

        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicate(
                clientId,
                serial,
                supplierIds,
                status,
                criteriaBuilder,
                itemRoot);
        criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }
}
