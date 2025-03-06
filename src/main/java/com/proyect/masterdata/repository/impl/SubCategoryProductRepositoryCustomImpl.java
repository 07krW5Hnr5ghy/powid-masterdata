package com.proyect.masterdata.repository.impl;

import com.proyect.masterdata.domain.CategoryProduct;
import com.proyect.masterdata.domain.SubCategoryProduct;
import com.proyect.masterdata.repository.SubCategoryProductRepositoryCustom;
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

@Repository
public class SubCategoryProductRepositoryCustomImpl implements SubCategoryProductRepositoryCustom {
    @PersistenceContext(name = "entityManager")
    private EntityManager entityManager;
    @Override
    public Page<SubCategoryProduct> searchForSubCategoryProduct(
            String name,
            String sku,
            String user,
            String categoryProduct,
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
        CriteriaQuery<SubCategoryProduct> criteriaQuery = criteriaBuilder.createQuery(SubCategoryProduct.class);
        Root<SubCategoryProduct> itemRoot = criteriaQuery.from(SubCategoryProduct.class);
        Join<SubCategoryProduct, CategoryProduct> subCategoryProductCategoryProductJoin = itemRoot.join("categoryProduct");

        criteriaQuery.select(itemRoot);
        List<Predicate> conditions = predicateConditions(
                name,
                sku,
                user,
                categoryProduct,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate,
                status,
                criteriaBuilder,
                itemRoot,
                subCategoryProductCategoryProductJoin);

        if (!StringUtils.isBlank(sort) && !StringUtils.isBlank(sortColumn)){
            List<Order> subCategoryProductList = new ArrayList<>();

            if (sort.equalsIgnoreCase("ASC")) {
                subCategoryProductList = listASC(sortColumn, criteriaBuilder, itemRoot);
            }

            if (sort.equalsIgnoreCase("DESC")) {
                subCategoryProductList = listDESC(sortColumn, criteriaBuilder, itemRoot);
            }

            criteriaQuery.where(conditions.toArray(new Predicate[] {})).orderBy(subCategoryProductList);
        }else{
            criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        }

        TypedQuery<SubCategoryProduct> ordeTypedQuery = entityManager.createQuery(criteriaQuery);
        ordeTypedQuery.setFirstResult(pageNumber * pageSize);
        ordeTypedQuery.setMaxResults(pageSize);

        Pageable pageable = PageRequest.of(pageNumber, pageSize);
        Long count = getOrderCount(name,sku, user,categoryProduct,registrationStartDate, registrationEndDate, updateStartDate, updateEndDate, status);
        return new PageImpl<>(ordeTypedQuery.getResultList(), pageable, count);
    }
    private List<Predicate> predicateConditions(
            String name,
            String sku,
            String user,
            String categoryProduct,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            Boolean status,
            CriteriaBuilder criteriaBuilder,
            Root<SubCategoryProduct> itemRoot,
            Join<SubCategoryProduct,CategoryProduct> subCategoryProductCategoryProductJoin
    ) {
        List<Predicate> conditions = new ArrayList<>();

        if(name != null){
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(itemRoot.get("name")),"%"+name.toUpperCase()+"%"));
        }

        if(sku != null){
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(itemRoot.get("sku")),"%"+sku.toUpperCase()+"%"));
        }

        if(categoryProduct!=null){
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(subCategoryProductCategoryProductJoin.get("name")),"%"+categoryProduct.toUpperCase()+"%"));
        }

        if (user != null) {
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.equal(
                                    criteriaBuilder.upper(itemRoot.get("tokenUser")), user.toUpperCase())));
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

    private List<Order> listASC(
            String sortColumn,
            CriteriaBuilder criteriaBuilder,
            Root<SubCategoryProduct> itemRoot
    ){
        List<Order> subCategoryProductList = new ArrayList<>();
        if (sortColumn.equalsIgnoreCase("NAME")) {
            subCategoryProductList.add(criteriaBuilder.asc(itemRoot.get("name")));
        }

        if (sortColumn.equalsIgnoreCase("tokenUser")) {
            subCategoryProductList.add(criteriaBuilder.asc(itemRoot.get("tokenUser")));
        }

        if (sortColumn.equalsIgnoreCase("registrationStartDate")) {
            subCategoryProductList.add(criteriaBuilder.asc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("registrationEndDate")) {
            subCategoryProductList.add(criteriaBuilder.asc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateStartDate")) {
            subCategoryProductList.add(criteriaBuilder.asc(itemRoot.get("updateDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateEndDate")) {
            subCategoryProductList.add(criteriaBuilder.asc(itemRoot.get("updateDate")));
        }

        return subCategoryProductList;
    }

    private List<Order> listDESC(
            String sortColumn,
            CriteriaBuilder criteriaBuilder,
            Root<SubCategoryProduct> itemRoot
    ){
        List<Order> subCategoryProductList = new ArrayList<>();
        if (sortColumn.equalsIgnoreCase("NAME")) {
            subCategoryProductList.add(criteriaBuilder.desc(itemRoot.get("name")));
        }

        if (sortColumn.equalsIgnoreCase("tokenUser")) {
            subCategoryProductList.add(criteriaBuilder.desc(itemRoot.get("tokenUser")));
        }

        if (sortColumn.equalsIgnoreCase("registrationStartDate")) {
            subCategoryProductList.add(criteriaBuilder.desc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("registrationEndDate")) {
            subCategoryProductList.add(criteriaBuilder.desc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateStartDate")) {
            subCategoryProductList.add(criteriaBuilder.desc(itemRoot.get("updateDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateEndDate")) {
            subCategoryProductList.add(criteriaBuilder.desc(itemRoot.get("updateDate")));
        }

        return subCategoryProductList;
    }

    private Long getOrderCount(
            String name,
            String sku,
            String user,
            String categoryProduct,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            Boolean status
    ){
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<SubCategoryProduct> itemRoot = criteriaQuery.from(SubCategoryProduct.class);
        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        Join<SubCategoryProduct, CategoryProduct> subCategoryProductCategoryProductJoin = itemRoot.join("categoryProduct");
        List<Predicate> conditions = predicateConditions(
                name,
                sku,
                user,
                categoryProduct,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate,
                status,
                criteriaBuilder,
                itemRoot,
                subCategoryProductCategoryProductJoin);
        criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }
}
