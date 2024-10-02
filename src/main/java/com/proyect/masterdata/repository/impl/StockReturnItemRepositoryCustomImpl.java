package com.proyect.masterdata.repository.impl;

import com.proyect.masterdata.domain.Purchase;
import com.proyect.masterdata.domain.StockReturn;
import com.proyect.masterdata.domain.StockReturnItem;
import com.proyect.masterdata.domain.SupplierProduct;
import com.proyect.masterdata.repository.StockReturnItemRepositoryCustom;
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
public class StockReturnItemRepositoryCustomImpl implements StockReturnItemRepositoryCustom {
    @PersistenceContext(name = "entityManager")
    private EntityManager entityManager;
    @Override
    public Page<StockReturnItem> searchForStockReturnItem(
            Long clientId,
            String serial,
            List<Long> supplierIds,
            String supplierProduct,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<StockReturnItem> criteriaQuery = criteriaBuilder.createQuery(StockReturnItem.class);
        Root<StockReturnItem> itemRoot = criteriaQuery.from(StockReturnItem.class);
        Join<StockReturnItem, StockReturn> stockReturnItemStockReturnJoin = itemRoot.join("stockReturn");
        Join<StockReturnItem, SupplierProduct> stockReturnItemSupplierProductJoin = itemRoot.join("supplierProduct");

        criteriaQuery.select(itemRoot);
        List<Predicate> conditions = predicate(
                clientId,
                serial,
                supplierIds,
                supplierProduct,
                criteriaBuilder,
                itemRoot,
                stockReturnItemStockReturnJoin,
                stockReturnItemSupplierProductJoin);

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

        TypedQuery<StockReturnItem> orderTypedQuery = entityManager.createQuery(criteriaQuery);
        orderTypedQuery.setFirstResult(pageNumber * pageSize);
        orderTypedQuery.setMaxResults(pageSize);

        Pageable pageable = PageRequest.of(pageNumber, pageSize);
        Long count = getOrderCount(
                clientId,
                serial,
                supplierIds,
                supplierProduct
                );
        return new PageImpl<>(orderTypedQuery.getResultList(), pageable, count);
    }

    public List<Predicate> predicate(
            Long clientId,
            String serial,
            List<Long> supplierIds,
            String supplierProduct,
            CriteriaBuilder criteriaBuilder,
            Root<StockReturnItem> itemRoot,
            Join<StockReturnItem,StockReturn> stockReturnItemStockReturnJoin,
            Join<StockReturnItem, SupplierProduct> stockReturnItemSupplierProductJoin) {

        List<Predicate> conditions = new ArrayList<>();

        if (clientId != null) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("clientId"), clientId)));
        }

        if (serial != null) {
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(stockReturnItemStockReturnJoin.get("serial")),"%"+serial.toUpperCase()+"%"));
        }

        if(!supplierIds.isEmpty()){
            conditions.add(criteriaBuilder.and(stockReturnItemStockReturnJoin.get("supplierId").in(supplierIds)));
        }

        if(supplierProduct!=null){
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(stockReturnItemSupplierProductJoin.get("serial")),"%"+supplierProduct.toUpperCase()+"%"));
        }

        return conditions;
    }

    private List<Order> listASC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<StockReturnItem> itemRoot) {

        List<Order> stockReturnList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("clientId")) {
            stockReturnList.add(criteriaBuilder.asc(itemRoot.get("clientId")));
        }

        if (sortColumn.equalsIgnoreCase("supplierProductId")) {
            stockReturnList.add(criteriaBuilder.asc(itemRoot.get("supplierProductId")));
        }

        return stockReturnList;
    }

    private List<Order> listDESC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<StockReturnItem> itemRoot) {

        List<Order> stockReturnList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("clientId")) {
            stockReturnList.add(criteriaBuilder.desc(itemRoot.get("clientId")));
        }

        if (sortColumn.equalsIgnoreCase("supplierProductId")) {
            stockReturnList.add(criteriaBuilder.desc(itemRoot.get("supplierProductId")));
        }

        return stockReturnList;
    }

    private Long getOrderCount(
            Long clientId,
            String serial,
            List<Long> supplierIds,
            String supplierProduct) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<StockReturnItem> itemRoot = criteriaQuery.from(StockReturnItem.class);
        Join<StockReturnItem, StockReturn> stockReturnItemStockReturnJoin = itemRoot.join("stockReturn");
        Join<StockReturnItem, SupplierProduct> stockReturnItemSupplierProductJoin = itemRoot.join("supplierProduct");

        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicate(
                clientId,
                serial,
                supplierIds,
                supplierProduct,
                criteriaBuilder,
                itemRoot,
                stockReturnItemStockReturnJoin,
                stockReturnItemSupplierProductJoin);
        criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }
}
